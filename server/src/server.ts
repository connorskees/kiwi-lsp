import {
	createConnection,
	TextDocuments,
	Diagnostic,
	DiagnosticSeverity,
	ProposedFeatures,
	InitializeParams,
	CompletionItem,
	CompletionItemKind,
	TextDocumentPositionParams,
	TextDocumentSyncKind,
	InitializeResult,
	DocumentDiagnosticReportKind,
	type DocumentDiagnosticReport,
	DiagnosticRelatedInformation,
	DefinitionParams,
	HoverParams,
	CancellationToken,
	Hover,
	Definition,
	ReferenceParams,
	Location,
	DocumentSymbolParams,
	DocumentSymbol,
	SymbolKind,
	DiagnosticTag,
	CodeActionParams,
	CodeActionKind,
	CodeAction,
	TextEdit,
} from 'vscode-languageserver/node';
import {
	Position,
	TextDocument
} from 'vscode-languageserver-textdocument';
import {
	camelCase, pascalCase, constantCase
} from 'change-case';

import { nativeTypes, parseSchema, tokenize } from './parser';
import { Schema, Definition as KiwiDefinition, Field, Token } from './schema';
import { KiwiParseError, isCamelCase, isInsideRange, isPascalCase, isScreamingSnakeCase } from './util';

const connection = createConnection(ProposedFeatures.all);

const documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);

let hasWorkspaceFolderCapability = false;
let hasDiagnosticRelatedInformationCapability = false;

connection.onInitialize((params: InitializeParams) => {
	const capabilities = params.capabilities;

	hasWorkspaceFolderCapability = !!(
		capabilities.workspace && !!capabilities.workspace.workspaceFolders
	);
	hasDiagnosticRelatedInformationCapability = !!(
		capabilities.textDocument &&
		capabilities.textDocument.publishDiagnostics &&
		capabilities.textDocument.publishDiagnostics.relatedInformation
	);

	const result: InitializeResult = {
		capabilities: {
			textDocumentSync: TextDocumentSyncKind.Incremental,
			completionProvider: {},
			diagnosticProvider: {
				interFileDependencies: false,
				workspaceDiagnostics: false
			},
			hoverProvider: true,
			definitionProvider: true,
			referencesProvider: true,
			documentSymbolProvider: true,
			codeActionProvider: {
				codeActionKinds: [CodeActionKind.QuickFix],
				resolveProvider: true,
			}
		}
	};
	if (hasWorkspaceFolderCapability) {
		result.capabilities.workspace = {
			workspaceFolders: {
				supported: true
			}
		};
	}
	return result;
});

connection.languages.diagnostics.on(async (params) => {
	const document = documents.get(params.textDocument.uri);
	return {
		kind: DocumentDiagnosticReportKind.Full,
		items: document ? await validateTextDocument(document) : []
	} satisfies DocumentDiagnosticReport;
});

documents.onDidChangeContent(change => validateTextDocument(change.document));

const files: Record<string, Schema> = {};

async function validateTextDocument(textDocument: TextDocument): Promise<Diagnostic[]> {
	const text = textDocument.getText();

	let schema: Schema | undefined;
	let errors: KiwiParseError[] = [];

	try {
		const [parsed, validateErrors] = parseSchema(text);
		files[textDocument.uri] = parsed;
		schema = parsed;
		errors = validateErrors;
	} catch (e: any) {
		errors.push(e);
	}

	const diagnostics: Diagnostic[] = errors.map((e) => ({
		message: e.message,
		range: e.range,
		relatedInformation: e.relatedInformation && hasDiagnosticRelatedInformationCapability ? [
			DiagnosticRelatedInformation.create({
				uri: textDocument.uri,
				range: e.relatedInformation.span,
			}, e.relatedInformation.message)
		] : undefined,
		severity: DiagnosticSeverity.Error,
		source: 'kiwi',
		data: e.errorKind,
	}));

	for (const e of errors) {
		if (!e.relatedInformation || !hasDiagnosticRelatedInformationCapability) {
			continue;
		}

		diagnostics.push({
			message: "First definition here",
			range: e.relatedInformation.span,
			relatedInformation: [DiagnosticRelatedInformation.create({ uri: textDocument.uri, range: e.range }, "Duplicated here")],
			severity: DiagnosticSeverity.Hint,
			source: 'kiwi',
			data: "invalid id"
		});
	}

	for (const def of schema?.definitions ?? []) {
		if (!isPascalCase(def.name)) {
			diagnostics.push({
				message: `${def.kind.toLowerCase()} names should be PascalCase`,
				range: def.nameSpan,
				severity: DiagnosticSeverity.Warning,
				source: 'kiwi',
				data: { kind: "change case", newText: pascalCase(def.name) },
			});
		}

		if (def.kind === 'ENUM') {
			for (const field of def.fields) {
				if (isScreamingSnakeCase(field.name)) {
					continue;
				}

				diagnostics.push({
					message: `enum variants should be SCREAMING_SNAKE_CASE`,
					range: field.nameSpan,
					severity: DiagnosticSeverity.Warning,
					source: 'kiwi',
					data: { kind: "change case", newText: constantCase(field.name) },
				});
			}
		} else {
			for (const field of def.fields) {
				if (field.isDeprecated) {
					diagnostics.push({
						message: 'field deprecated',
						range: field.nameSpan,
						tags: [DiagnosticTag.Deprecated],
						severity: DiagnosticSeverity.Hint,
						source: 'kiwi',
					});
				}

				if (!isCamelCase(field.name)) {
					diagnostics.push({
						message: "field names should be camelCase",
						range: field.nameSpan,
						severity: DiagnosticSeverity.Warning,
						source: 'kiwi',
						data: { kind: "change case", newText: camelCase(field.name) },
					});
				}
			}
		}
	}

	if (schema?.package && !isPascalCase(schema.package.text)) {
		diagnostics.push({
			message: "package names should be PascalCase",
			range: schema.package.span,
			severity: DiagnosticSeverity.Warning,
			source: 'kiwi',
			data: { kind: "change case", newText: pascalCase(schema.package.text) },
		});
	}

	return diagnostics;
}

const PRIMITIVE_TYPE_DOCS: Record<string, string> = {
	'bool': 'A value that stores either true or false',
	'byte': 'An unsigned 8-bit integer value',
	'int': 'A signed 32-bit integer',
	'uint': 'An unsigned 32-bit integer',
	'int64': 'A signed 64-bit integer',
	'float': 'A 32-bit floating-point number',
	'uint64': 'An unsigned 64-bit integer',
	'string': 'A UTF-8 null-terminated string',
};

const KEYWORD_DOCS: Record<string, string> = {
	'enum': '',
	'struct': 'An object whose fields are required. More space efficient than a message, but should be used sparingly due to back-compat.',
	'message': 'An object whose fields are optional. The default structure in a kiwi document.'
};

function getSchema(uri: string): Schema | undefined {
	try {
		const document = documents.get(uri);
		const [parsed, errors] = parseSchema(document!.getText());
		files[uri] = parsed;
	} catch {
		//
	}

	return files[uri];
}

connection.onReferences((params: ReferenceParams): Location[] => {
	const locs: Location[] = [];

	const schema = getSchema(params.textDocument.uri);

	if (!schema) {
		return [];
	}

	let target: string | undefined;
	let targetLoc: Location | undefined;

	for (const def of schema.definitions) {
		if (isInsideRange(params.position, def.nameSpan)) {
			target = def.name;
			targetLoc = { uri: params.textDocument.uri, range: def.nameSpan };
			break;
		}

		if (def.kind === 'ENUM') {
			continue;
		}

		if (isInsideRange(params.position, def.fieldsSpan)) {
			for (const field of def.fields) {
				if (isInsideRange(params.position, field.typeSpan)) {
					target = field.type;
					targetLoc = { uri: params.textDocument.uri, range: field.typeSpan! };
					break;
				}
			}

			break;
		}
	}

	if (!target) {
		return [];
	}

	for (const def of schema.definitions) {
		if (def.kind === 'ENUM') {
			continue;
		}

		for (const field of def.fields) {
			if (field.type !== target) {
				continue;
			}

			locs.push({
				uri: params.textDocument.uri,
				range: field.typeSpan!,
			});
		}
	}

	if (params.context.includeDeclaration && targetLoc) {
		locs.push(targetLoc);
	}

	return locs;
});

function findContainingDefinition(position: Position, schema: Schema): KiwiDefinition | undefined {
	return schema.definitions.find(def => isInsideRange(position, def.defSpan));
}

connection.onHover((params: HoverParams, token: CancellationToken): Hover => {
	const schema = getSchema(params.textDocument.uri);

	if (!schema) {
		return { contents: [] };
	}

	const formatForDef = (def: KiwiDefinition): string[] => {
		const notes = [`(${def.kind.toLowerCase()}) ${def.name}`];

		if (def.fields.length <= 3) {
			return notes;
		}

		if (def.kind === 'ENUM') {
			notes.push(`${def.fields.length} variants`);
		} else {
			notes.push(`${def.fields.length} fields | next id ${def.fields.length + 1}`);
		}

		return notes;
	};

	const def = findContainingDefinition(params.position, schema);

	if (!def) {
		return { contents: [] };
	}

	if (isInsideRange(params.position, def.keywordSpan)) {
		return { contents: KEYWORD_DOCS[def.kind.toLowerCase()] };
	}

	if (isInsideRange(params.position, def.nameSpan)) {
		return { contents: formatForDef(def), range: def.nameSpan };
	}

	let target: Field | undefined;

	if (isInsideRange(params.position, def.fieldsSpan)) {
		for (const field of def.fields) {
			if (isInsideRange(params.position, field.typeSpan)) {
				if (PRIMITIVE_TYPE_DOCS[field.type!]) {
					return { contents: `(builtin) ${PRIMITIVE_TYPE_DOCS[field.type!]}`, range: field.typeSpan };
				}

				target = field;

				break;
			}

			if (isInsideRange(params.position, field.deprecatedSpan)) {
				return { contents: ['deprecated fields are ignored during codegen'] }
			}

			if (isInsideRange(params.position, field.nameSpan)) {
				break;
			}
		}
	}

	if (target) {
		const def = schema.definitions.find(def => def.name === target?.type);

		if (def) {
			return { contents: `(${def.kind.toLowerCase()}) ${def.name}`, range: target.typeSpan };
		}
	}

	return { contents: [] };
});

connection.onDefinition((params: DefinitionParams): Definition | undefined => {
	const schema = getSchema(params.textDocument.uri);

	if (!schema) {
		return;
	}

	let tokenInside: string | undefined;

	const containingDef = findContainingDefinition(params.position, schema);

	if (!containingDef) {
		return;
	}

	if (isInsideRange(params.position, containingDef.nameSpan)) {
		return { uri: params.textDocument.uri, range: containingDef.nameSpan };
	}

	if (isInsideRange(params.position, containingDef.fieldsSpan)) {
		for (const field of containingDef.fields) {
			if (isInsideRange(params.position, field.typeSpan)) {
				tokenInside = field.type;
				break;
			}
		}
	}

	if (!tokenInside) {
		return;
	}

	const def = schema.definitions.find(def => def.name === tokenInside);

	return def && { range: def.nameSpan, uri: params.textDocument.uri };
});

connection.onDocumentSymbol((params: DocumentSymbolParams): DocumentSymbol[] => {
	const schema = getSchema(params.textDocument.uri);

	if (!schema) {
		return [];
	}

	const symbols: DocumentSymbol[] = [];

	if (schema.package) {
		symbols.push({
			name: schema.package.text,
			kind: SymbolKind.Package,
			range: schema.package.span,
			selectionRange: schema.package.span,
		});
	}

	for (const def of schema.definitions) {
		symbols.push({
			name: def.name,
			kind: def.kind === 'ENUM' ? SymbolKind.Enum : SymbolKind.Class,
			range: def.defSpan,
			selectionRange: def.nameSpan,
		});

		for (const field of def.fields) {
			symbols.push({
				name: field.name,
				kind: def.kind === 'ENUM' ? SymbolKind.EnumMember : SymbolKind.Field,
				range: field.span,
				selectionRange: field.nameSpan,
			});
		}
	}

	return symbols;
});

function getNextId(def: KiwiDefinition): number | undefined {
	if (def.kind !== 'MESSAGE') {
		return;
	}

	const usedIds = new Set(def.fields.map(f => f.value));

	for (let i = 1; i < def.fields.length; i++) {
		if (!usedIds.has(i)) {
			return i;
		}
	}

	return def.fields.length;
}

connection.onCodeAction((params: CodeActionParams): CodeAction[] => {
	const diagnostics = params.context.diagnostics.filter(d => !!d.data);

	if (diagnostics.length === 0) {
		return [];
	}

	const schema = getSchema(params.textDocument.uri);

	if (!schema) {
		return [];
	}

	const def = findContainingDefinition(params.range.start, schema);

	function getNextIdQuickActions() {
		const nextIdDiagnostics = diagnostics.filter(d => d.data === "invalid id");

		if (!def || def.kind !== 'MESSAGE') {
			return [];
		}

		return nextIdDiagnostics.map(diagnostic => ({
			title: 'Use next available id',
			kind: CodeActionKind.QuickFix,
			diagnostics: [diagnostic],
			edit: {
				changes: {
					[params.textDocument.uri]: [TextEdit.replace(diagnostic.range, `${getNextId(def)}`)],
				}
			}
		}));
	}

	function getChangeCaseQuickActions() {
		const changeCaseDiagnostics = diagnostics.filter(d => d.data?.kind === "change case");

		if (!def) {
			return [];
		}

		return changeCaseDiagnostics.map(diagnostic => ({
			title: 'Change case',
			kind: CodeActionKind.QuickFix,
			diagnostics: [diagnostic],
			edit: {
				changes: {
					[params.textDocument.uri]: [TextEdit.replace(diagnostic.range, diagnostic.data?.newText)],
				}
			}
		}));
	}

	return [
		...getNextIdQuickActions(),
		...getChangeCaseQuickActions(),
	]
});

connection.onCompletion(
	(textDocumentPosition: TextDocumentPositionParams): CompletionItem[] => {
		const schema = getSchema(textDocumentPosition.textDocument.uri);

		if (!schema) {
			return [];
		}

		const typeCompletions = [
			...nativeTypes.map(ty => ({
				label: ty,
				kind: CompletionItemKind.Field,
			})),
			...schema.definitions.map(d => ({
				label: d.name,
				kind: d.kind === 'ENUM'
					? CompletionItemKind.Enum
					: d.kind === 'STRUCT'
						? CompletionItemKind.Interface
						: CompletionItemKind.Class
			})),
		];

		const toplevelCompletions = ['message ', 'struct ', 'enum '].map(kw => ({
			label: kw,
			kind: CompletionItemKind.Keyword,
		}));

		if (
			!schema.package && (schema.definitions.length === 0
				|| textDocumentPosition.position.line < schema.definitions[0]!.nameSpan.start.line)
		) {
			toplevelCompletions.push({
				label: 'package ',
				kind: CompletionItemKind.Keyword,
			});
		}

		const docContents = documents.get(textDocumentPosition.textDocument.uri)?.getText();
		if (!docContents) {
			return [];
		}

		const [tokens, _] = tokenize(docContents);

		for (const def of schema.definitions) {
			if (isInsideRange(textDocumentPosition.position, def.fieldsSpan)) {
				if (def.kind === 'ENUM') {
					return [];
				}

				let prevTok: Token | undefined;

				for (const tok of tokens) {
					if (
						tok.span.end.line === textDocumentPosition.position.line
						&& tok.span.end.character === textDocumentPosition.position.character
					) {
						break;
					}

					prevTok = tok;
				}

				if (!prevTok || prevTok.text === ';' || prevTok.text === '{') {
					return typeCompletions;
				}

				return [
					{
						label: 'deprecated',
						kind: CompletionItemKind.Keyword,
					}
				];
			}
		}

		return toplevelCompletions;
	}
);

documents.listen(connection);
connection.listen();
