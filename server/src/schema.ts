import { Range } from 'vscode-languageserver';

export interface Token {
	text: string
	span: Range
}

export interface Schema {
	package: Token | null;
	definitions: Definition[];
}

export type DefinitionKind = 'ENUM' | 'STRUCT' | 'MESSAGE';

export interface Definition {
	keywordSpan: Range;
	/**
	 * Span from start of keyword to closing curly brace
	 */
	defSpan: Range;
	name: string;
	nameSpan: Range;
	kind: DefinitionKind;
	fields: Field[];
	fieldsSpan: Range;
}

export interface Field {
	/**
	 * Span from type to semicolon
	 */
	span: Range;
	name: string;
	nameSpan: Range;
	type: string | undefined;
	typeSpan: Range | undefined;
	isArray: boolean;
	isDeprecated: boolean;
	deprecatedSpan?: Range;
	value: number;
	valueSpan: Range;
}