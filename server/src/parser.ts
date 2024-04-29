import { Range } from 'vscode-languageserver';
import { Schema, Definition, Field, DefinitionKind, Token } from "./schema";
import { KiwiParseError, combineRanges, createError, endOfRange, error, quote } from './util';

export let nativeTypes = [
	'bool',
	'byte',
	'float',
	'int',
	'int64',
	'string',
	'uint',
	'uint64',
];

// These are special names on the object returned by compileSchema()
export let reservedNames = [
	'ByteBuffer',
	'package',
	'enum',
	'struct',
	'message',
	...nativeTypes,
];

let regex = /((?:-|\b)\d+\b|[=;{}]|\[\]|\[deprecated\]|\b[A-Za-z_][A-Za-z0-9_]*\b|\/\/.*|\s+)/g;
let identifier = /^[A-Za-z_][A-Za-z0-9_]*$/;
let whitespace = /^\/\/.*|\s+$/;
let equals = /^=$/;
let endOfFile = /^$/;
let semicolon = /^;$/;
let integer = /^-?\d+$/;
let leftBrace = /^\{$/;
let rightBrace = /^\}$/;
let arrayToken = /^\[\]$/;
let enumKeyword = /^enum$/;
let structKeyword = /^struct$/;
let messageKeyword = /^message$/;
let packageKeyword = /^package$/;
let deprecatedToken = /^\[deprecated\]$/;

export function tokenize(text: string): [Token[], KiwiParseError[]] {
	const errors: KiwiParseError[] = []

	let parts = text.split(regex);
	let tokens: Token[] = [];
	let column = 0;
	let line = 0;

	for (let i = 0; i < parts.length; i++) {
		let part = parts[i];

		// Keep non-whitespace tokens
		if (i & 1) {
			if (!whitespace.test(part)) {
				tokens.push({
					text: part,
					span: {
						start: { line, character: column },
						end: { line, character: column + part.length }
					}
				});
			}
		}

		// Detect syntax errors
		else if (part !== '') {
			errors.push(createError(`invalid token: ${quote(part)}`, {
				start: { line, character: column },
				end: { line, character: column + part.length },
			}))
		}

		// Keep track of the line and column counts
		let lines = part.split('\n');
		if (lines.length > 1) column = 0;
		line += lines.length - 1;
		column += lines[lines.length - 1].length;
	}

	// End-of-file token
	tokens.push({
		text: '',
		span: {
			start: { line, character: column },
			end: { line, character: column }
		},
	});

	return [tokens, errors];
}

function parse(tokens: Token[]): [Schema, KiwiParseError[]] {
	const errors: KiwiParseError[] = []
	function current(): Token {
		return tokens[index];
	}

	function eat(test: RegExp): Token | null {
		const token = current();
		if (test.test(token.text)) {
			index++;
			return token;
		}
		return null;
	}

	function expect(test: RegExp, expected: string): Token {
		const token = current();
		if (!eat(test)) {
			error('Expected ' + expected + ' but found ' + quote(token.text), token.span);
		}

		return token;
	}

	let definitions: Definition[] = [];
	let packageIdent = null;
	let index = 0;

	if (eat(packageKeyword)) {
		packageIdent = eat(identifier);

		const packageSemi = eat(semicolon);
		if (packageIdent) {
			if (!packageSemi) {
				errors.push(createError('expected ";"', endOfRange(packageIdent.span)))
			}
		} else {
			errors.push(createError('expected package name', endOfRange(current().span)))
		}
	}

	while (index < tokens.length && !eat(endOfFile)) {
		let fields: Field[] = [];
		let kind: DefinitionKind;
		let kindTok: Token | null;

		if (kindTok = eat(enumKeyword)) kind = 'ENUM';
		else if (kindTok = eat(structKeyword)) kind = 'STRUCT';
		else if (kindTok = eat(messageKeyword)) kind = 'MESSAGE';
		else {
			const tok = current();
			errors.push(createError(`unexpected token "${tok.text}"`, tok.span));
			index += 1;
			continue;
		}

		// All definitions start off the same
		let name: Token | null = current();

		if (!eat(identifier)) {
			name = null;
			errors.push(createError(`${kind.toLowerCase()} missing name`, combineRanges(kindTok.span, current().span)));
		}

		const openFieldsBrace = expect(leftBrace, '"{"');
		let closeFieldsBrace: Token | null;

		// Parse fields
		while (true) {
			closeFieldsBrace = eat(rightBrace)

			if (closeFieldsBrace) {
				break;
			}

			let type: Token | null = null;
			let isArray = false;
			let isDeprecated = false;

			// Enums don't have types
			if (kind !== 'ENUM') {
				type = current();
				expect(identifier, 'identifier');
				isArray = !!eat(arrayToken);
			}

			let field = current();
			expect(identifier, 'identifier');

			// Structs don't have explicit values
			let value: Token | null = null;
			if (kind !== 'STRUCT') {
				let eqError;
				if (!eat(equals)) {
					eqError = createError('Expected "="', current().span);
				}

				value = current();

				if (!eat(integer)) {
					if (eqError) {
						eqError.message = `Expected id assignment. e.g. \`${type ? type.text + ' ' : ''}${field.text} = 1\``
					} else {
						eqError = createError(`Expected integer id. found ${quote(current().text)}`, current().span);
					}
				}

				if (eqError) {
					errors.push(eqError);
				} else if ((+value.text | 0) + '' !== value.text) {
					errors.push(createError('Invalid integer ' + quote(value.text), current().span));
				}
			}

			let deprecated = current();
			if (eat(deprecatedToken)) {
				if (kind !== 'MESSAGE') {
					errors.push(createError('Cannot deprecate this field', deprecated.span));
				}

				isDeprecated = true;
			} else if (kind !== 'ENUM') {
				let deprecatedErrorToken = null;
				if (deprecatedErrorToken = eat(/\[|\]/)) {
					errors.push(createError('Expected `[deprecated]`', deprecatedErrorToken.span));
				} else if (deprecatedErrorToken = eat(/deprecated/)) {
					errors.push(createError('Did you mean `[deprecated]`?', deprecatedErrorToken.span));
				}
			}

			let semicolonSpan;

			if (kind === 'STRUCT' && current().text === '=') {
				let range = current().span;
				expect(equals, '"="');

				let next;
				if (next = eat(integer)) {
					range = combineRanges(range, next.span);
				}
				semicolonSpan = eat(semicolon)?.span;

				errors.push(createError('Struct fields can\'t have explicit ids.', range));
			}

			if (!eat(semicolon)) {
				const range = value?.span ?? field.span;
				errors.push(createError('Expected ";"', endOfRange(range)));
			}

			if (name?.text === 'WidgetHoverStyle') {
				console.log({ c: current() });
			}

			const fullSpan = combineRanges(field.span, type?.span || field.span, value?.span || field.span, semicolonSpan || field.span);

			fields.push({
				span: fullSpan,
				name: field.text,
				nameSpan: field.span,
				type: type?.text,
				typeSpan: type?.span,
				isArray: isArray,
				isDeprecated: isDeprecated,
				deprecatedSpan: deprecated.span,
				value: value !== null ? +value.text | 0 : fields.length + 1,
				valueSpan: value?.span || fullSpan,
			});
		}

		if (!name) {
			continue;
		}

		definitions.push({
			keywordSpan: kindTok.span,
			defSpan: combineRanges(kindTok.span, closeFieldsBrace.span),
			name: name.text,
			nameSpan: name.span,
			kind: kind,
			fields: fields,
			fieldsSpan: combineRanges(openFieldsBrace.span, closeFieldsBrace.span)
		});
	}

	return [{
		package: packageIdent,
		definitions: definitions,
	}, errors];
}

function verify(root: Schema): KiwiParseError[] {
	const errors: KiwiParseError[] = []

	const reservedTypeNames = [...nativeTypes]

	let definedTypes = nativeTypes.slice();
	let definitions: { [name: string]: Definition } = {};

	// Define definitions
	for (let i = 0; i < root.definitions.length; i++) {
		let definition = root.definitions[i];

		const nativeDefinition = nativeTypes.find(v => v === definition.name);
		if (nativeDefinition) {
			errors.push(createError('The type ' + quote(definition.name) + ' conflicts with a native type', definition.nameSpan));
		}

		const duplicateDefinitionIdx = definedTypes.indexOf(definition.name);
		if (duplicateDefinitionIdx !== -1) {
			errors.push(createError('The type ' + quote(definition.name) + ' is defined twice', definition.nameSpan, { message: "Also defined here", span: definitions[definedTypes[duplicateDefinitionIdx]].nameSpan }));
		}
		if (reservedNames.indexOf(definition.name) !== -1) {
			errors.push(createError('The type name ' + quote(definition.name) + ' is reserved', definition.nameSpan));
		}
		definedTypes.push(definition.name);
		definitions[definition.name] = definition;
	}

	// Check fields
	for (let i = 0; i < root.definitions.length; i++) {
		let definition = root.definitions[i];
		let fields = definition.fields;

		if (fields.length === 0) {
			continue;
		}

		if (definition.kind !== 'ENUM') {
			// Check types
			for (let j = 0; j < fields.length; j++) {
				let field = fields[j];
				if (definedTypes.indexOf(field.type!) === -1) {
					errors.push(createError('The type ' + quote(field.type!) + ' doesn\'t exist.', field.typeSpan!));
				}
			}
		}

		// Check values
		const checkedFields: Field[] = [];
		for (let j = 0; j < fields.length; j++) {
			let field = fields[j];

			const duplicateName = checkedFields.find(f => f.name === field.name);
			if (duplicateName) {
				errors.push(createError('The name for field ' + quote(field.name) + ' is used twice', field.nameSpan, { message: "Also used here", span: duplicateName.nameSpan }));
			}

			const duplicateValue = checkedFields.find(f => f.value === field.value);
			let fieldError;
			if (duplicateValue) {
				fieldError = createError('The id for field ' + quote(field.name) + ' is used twice', field.valueSpan, { message: "Also used here", span: duplicateValue.valueSpan }, "invalid id");
			} else if ((definition.kind === 'ENUM' && field.value < 0) || (definition.kind !== 'ENUM' && field.value <= 0)) {
				fieldError = createError('The id for field ' + quote(field.name) + ' must be positive', field.valueSpan, undefined, "invalid id");
			} else if (definition.kind !== 'ENUM' && field.value > fields.length) {
				fieldError = createError('The id for field ' + quote(field.name) + ' cannot be larger than ' + fields.length, field.valueSpan, undefined, "invalid id");
			}

			if (fieldError) {
				errors.push(fieldError);
			}

			checkedFields.push(field);
		}
	}

	// Check that structs don't contain themselves
	let state: { [name: string]: number } = {};
	let check = (name: string): boolean => {
		let definition = definitions[name];
		if (definition && definition.kind === 'STRUCT') {
			if (state[name] === 1) {
				errors.push(createError('Recursive nesting of ' + quote(name) + ' is not allowed', definition.nameSpan));
			}
			if (state[name] !== 2 && definition) {
				state[name] = 1;
				let fields = definition.fields;
				for (let i = 0; i < fields.length; i++) {
					let field = fields[i];
					if (!field.isArray) {
						check(field.type!);
					}
				}
				state[name] = 2;
			}
		}
		return true;
	};
	for (let i = 0; i < root.definitions.length; i++) {
		check(root.definitions[i].name);
	}

	return errors;
}

export function parseSchema(text: string): [Schema, KiwiParseError[]] {
	const [tokens, tokenErrors] = tokenize(text);
	const [schema, parseErrors] = parse(tokens);
	const validateErrors = verify(schema);
	return [schema, [...tokenErrors, ...parseErrors, ...validateErrors]];
}