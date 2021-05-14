import { Chunk, parse } from "../src";

export const testParse = (...args: Parameters<typeof parse>): (() => Chunk) => {
    return parse.bind(null, ...args);
};
