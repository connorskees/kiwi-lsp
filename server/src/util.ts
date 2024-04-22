import { Position, Range } from 'vscode-languageserver';

export function quote(text: string): string {
	return JSON.stringify(text);
}

export class KiwiParseError extends Error {
	constructor(message: string, public readonly range: Range, public readonly relatedInformation?: RelatedInformation) {
		super(message)
	}
}

interface RelatedInformation {
	span: Range;
	message: string
}

export function combineRanges(...ranges: Range[]) {
	let start = { ...ranges[0].start };
	let end = { ...ranges[0].end };

	for (const r of ranges) {
		if (r.start.line < start.line) {
			start = { ...r.start }
		} else if (r.start.line === start.line) {
			start.character = Math.min(start.character, r.start.character)
		}

		if (r.end.line > end.line) {
			end = { ...r.end };
		} else if (r.end.line === end.line) {
			end.character = Math.max(end.character, r.end.character);
		}
	}

	return { start, end }
}

export function endOfRange(range: Range): Range {
	return {
		start: { line: range.end.line, character: range.end.character },
		end: { line: range.end.line, character: range.end.character + 1 },
	}
}

// export function error(text: string, line: number, column: number): never;
export function error(text: string, range: Range, relatedInformation?: RelatedInformation): never {
	throw new KiwiParseError(text, range, relatedInformation);
}

export function createError(text: string, range: Range, relatedInformation?: RelatedInformation): KiwiParseError {
	return new KiwiParseError(text, range, relatedInformation);
}

export function isPascalCase(s: string): boolean {
	return s[0] >= 'A' && s[0] <= 'Z' && !s.includes('_');
}

export function isCamelCase(s: string): boolean {
	return s[0] >= 'a' && s[0] <= 'z' && !s.includes('_');
}

export function isScreamingSnakeCase(s: string): boolean {
	if (!(s[0] >= 'A' && s[0] <= 'Z')) {
		return false;
	}

	for (const c of s) {
		if ((c >= 'A' && c <= 'Z') || c === '_' || (c >= '0' && c <= '9')) {
			continue;
		}

		return false;
	}

	return true;
}

export function isInsideRange(position: Position, range: Range | undefined): boolean {
	if (!range || range.start.line > position.line
		|| range.end.line < position.line) {
		return false
	}

	if (range.start.line === position.line && range.end.line === position.line) {
		return range.start.character < position.character && range.end.character > position.character;
	}

	if (range.start.line === position.line) {
		return range.start.character < position.character;
	}

	if (range.end.line === position.line) {
		return range.end.character > position.character;
	}

	return true;
}
