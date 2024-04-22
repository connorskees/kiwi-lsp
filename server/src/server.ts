/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

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
} from 'vscode-languageserver/node';

import {
	Position,
	TextDocument
} from 'vscode-languageserver-textdocument';

import { nativeTypes, parseSchema } from './parser';
import { Schema, Definition as KiwiDefinition, Field, Token } from './schema';
import { KiwiParseError, combineRanges, isCamelCase, isInsideRange, isPascalCase, isScreamingSnakeCase } from './util';

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
			completionProvider: {
				resolveProvider: true
			},
			diagnosticProvider: {
				interFileDependencies: false,
				workspaceDiagnostics: false
			},
			hoverProvider: true,
			definitionProvider: true,
			referencesProvider: true,
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

// The content of a text document has changed. This event is emitted
// when the text document first opened or when its content has changed.
documents.onDidChangeContent(change => {
	validateTextDocument(change.document);
});

const files: Record<string, { parsed: Schema, tokens: Token[] }> = {}

async function validateTextDocument(textDocument: TextDocument): Promise<Diagnostic[]> {
	const text = textDocument.getText();

	let schema: Schema | undefined;
	let errors: KiwiParseError[] = [];

	try {
		const [parsed, tokens, validateErrors] = parseSchema(text);
		files[textDocument.uri] = { parsed, tokens };
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
		source: 'kiwi'
	}))

	for (const e of errors) {
		if (e.relatedInformation && hasDiagnosticRelatedInformationCapability) {
			diagnostics.push({
				message: "First definition here",
				range: e.relatedInformation.span,
				relatedInformation: [DiagnosticRelatedInformation.create({ uri: textDocument.uri, range: e.range }, "Duplicated here")],
				severity: DiagnosticSeverity.Hint,
				source: 'kiwi'
			})
		}
	}

	for (const def of schema?.definitions ?? []) {
		if (!isPascalCase(def.name)) {
			diagnostics.push({
				message: `${def.kind.toLowerCase()} names should be PascalCase`,
				range: def.nameSpan,
				severity: DiagnosticSeverity.Warning,
				source: 'kiwi'
			});
		}

		if (def.kind === 'ENUM') {
			for (const field of def.fields) {
				if (!isScreamingSnakeCase(field.name)) {
					diagnostics.push({
						message: `enum variants should be SCREAMING_SNAKE_CASE`,
						range: field.nameSpan,
						severity: DiagnosticSeverity.Warning,
						source: 'kiwi'
					});
				}
			}
		} else {
			for (const field of def.fields) {
				if (!isCamelCase(field.name)) {
					diagnostics.push({
						message: "field names should be camelCase",
						range: field.nameSpan,
						severity: DiagnosticSeverity.Warning,
						source: 'kiwi'
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
			source: 'kiwi'
		});
	}

	return diagnostics;
}

connection.onDidChangeWatchedFiles(_change => {
	// Monitored files have change in VSCode
	connection.console.log('We received a file change event');
});

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
}

function getSchema(uri: string): { parsed: Schema; tokens: Token[] } | undefined {
	if (files[uri]) {
		return files[uri];
	}

	try {
		const document = documents.get(uri);
		const [parsed, tokens, errors] = parseSchema(document?.getText()!);
		files[uri] = { parsed, tokens };
		return files[uri];
	} catch {
		return undefined;
	}
}

connection.onReferences((params: ReferenceParams): Location[] => {
	const locs: Location[] = []

	const schema = getSchema(params.textDocument.uri)?.parsed;

	if (!schema) {
		return [];
	}

	let target: string | undefined;
	let targetLoc: Location | undefined;

	for (const def of schema.definitions) {
		if (isInsideRange(params.position, def.nameSpan)) {
			target = def.name;
			targetLoc = { uri: params.textDocument.uri, range: def.nameSpan }
			break;
		}

		if (def.kind === 'ENUM') {
			continue;
		}

		if (isInsideRange(params.position, def.fieldsSpan)) {
			for (const field of def.fields) {
				if (isInsideRange(params.position, field.typeSpan)) {
					target = field.type;
					targetLoc = { uri: params.textDocument.uri, range: field.typeSpan! }
					// if (PRIMITIVE_TYPE_DOCS[field.type!]) {
					// 	return { contents: `(builtin) ${PRIMITIVE_TYPE_DOCS[field.type!]}`, range: field.typeSpan };
					// }
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
			if (field.type === target) {
				locs.push({
					uri: params.textDocument.uri,
					range: field.typeSpan!,
				})
			}
		}
	}

	if (params.context.includeDeclaration) {
		locs.push(targetLoc!)
	}

	return locs;
});

function findContainingDefinition(position: Position, schema: Schema): KiwiDefinition | undefined {
	return schema.definitions.find(def => isInsideRange(position, def.defSpan));
}

function findContainingField(position: Position, schema: Schema): Field | undefined {
	return findContainingDefinition(position, schema)?.fields.find(field => isInsideRange(position, field.span));
}

connection.onHover((params: HoverParams, token: CancellationToken): Hover => {
	const schema = getSchema(params.textDocument.uri)?.parsed;

	if (!schema) {
		return { contents: [] };
	}

	const def = findContainingDefinition(params.position, schema);

	if (!def) {
		return { contents: [] };
	}

	if (isInsideRange(params.position, def.keywordSpan)) {
		return { contents: KEYWORD_DOCS[def.kind.toLowerCase()] };
	}

	if (isInsideRange(params.position, def.nameSpan)) {
		return { contents: `(${def.kind.toLowerCase()}) ${def.name}`, range: def.nameSpan };
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

			if (isInsideRange(params.position, field.nameSpan)) {
				break;
				// if (def.kind === 'ENUM') {
				// 	return { contents: `(variant) ${def.name}.${field.name}`, range: field.nameSpan }
				// }


				// return { contents: `(field) ${def.name}.${field.name}`, range: field.nameSpan }
			}
		}
	}

	if (target) {
		const def = schema.definitions.find(def => def.name === target?.type);

		if (def) {
			return { contents: `(${def.kind.toLowerCase()}) ${def.name}` }
		}
	}

	return { contents: [] };
})

connection.onDefinition((params: DefinitionParams): Definition | undefined => {
	const schema = getSchema(params.textDocument.uri)?.parsed;

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

	const symbols: DocumentSymbol[] = []

	for (const def of schema.parsed.definitions) {
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

// This handler provides the initial list of the completion items.
connection.onCompletion(
	(textDocumentPosition: TextDocumentPositionParams): CompletionItem[] => {
		const parsed = getSchema(textDocumentPosition.textDocument.uri);

		if (!parsed) {
			return [];
		}

		const schema = parsed.parsed;
		const tokens = parsed.tokens;

		const typeCompletions = [
			...nativeTypes.map(ty => ({
				label: ty + ' ',
				kind: CompletionItemKind.Field,
			})),
			...schema.definitions.map(d => ({
				label: d.name + ' ',
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

		if (!schema.package && (schema.definitions.length === 0 || textDocumentPosition.position.line < schema.definitions[0]!.nameSpan.start.line)) {
			toplevelCompletions.push({
				label: 'package ',
				kind: CompletionItemKind.Keyword,
			})
		}

		for (const def of schema.definitions) {
			const span = def.fieldsSpan;
			if (isInsideRange(textDocumentPosition.position, span)) {
				if (def.kind === 'ENUM') {
					return [];
				}

				// const containingField = findContainingField(textDocumentPosition.position, schema);

				// if (containingField && (isInsideRange(textDocumentPosition.position, containingField.nameSpan) || isInsideRange(textDocumentPosition.position, containingField.valueSpan))) {
				// 	return [];
				// }

				// let prevTok: Token | undefined;

				// for (const tok of tokens) {
				// 	if (tok.span.start.line > textDocumentPosition.position.line) {
				// 		break;
				// 	}

				// 	if (tok.span.start.line === textDocumentPosition.position.line && tok.span.start.character < textDocumentPosition.position.character) {
				// 		break;
				// 	}

				// 	prevTok = tok;
				// }

				// // const tokenIdx = tokens.findIndex(t => t.span.end.line === textDocumentPosition.position.line && t.span.end.character === textDocumentPosition.position.character);
				// // const prevTok = tokens[tokenIdx - 1];
				// console.log({ prevTok, pos: textDocumentPosition.position, tokens })
				// if (!prevTok || prevTok.text === ';' || prevTok.text === '{') {
				// 	return typeCompletions;
				// }

				return typeCompletions
			}
		}

		return toplevelCompletions;
	}
);

// This handler resolves additional information for the item selected in
// the completion list.
connection.onCompletionResolve(
	(item: CompletionItem): CompletionItem => {
		return item;
	}
);

// Make the text document manager listen on the connection
// for open, change and close text document events
documents.listen(connection);

// Listen on the connection
connection.listen();
