import { expect } from "chai";
import { testParse } from "./lib";

describe("Assignments", () => {
    it("Throw '=' expected error", () => {
        expect(testParse("a = a b")).to.throw(
            "[1:7] '=' expected near '<eof>'"
        );
    });
    it("Throw unexpected number", () => {
        expect(testParse("a = 1 2")).to.throw(
            "[1:6] unexpected number '2' near '<eof>'"
        );
        expect(testParse("0 = 0")).to.throw(
            "[1:0] unexpected number '0' near '='"
        );
    });
    it("Throw unexpected string", () => {
        expect(testParse('"foo" = 0')).to.throw(
            "[1:0] unexpected string '\"foo\"' near '='"
        );
    });
    it("Throw unexpected symbol", () => {
        expect(testParse("a = a = 1")).to.throw(
            "[1:6] unexpected symbol '=' near '1'"
        );
        expect(testParse("(a) = 0")).to.throw(
            "[1:4] unexpected symbol '=' near '0'"
        );
        expect(testParse("{} = 0")).to.throw(
            "[1:0] unexpected symbol '{' near '}'"
        );
        expect(testParse("a, b(), c")).to.throw(
            "[1:6] unexpected symbol ',' near 'c'"
        );
        expect(testParse("a:b() = 0")).to.throw(
            "[1:6] unexpected symbol '=' near '0'"
        );
        expect(testParse("a[b]() = 0")).to.throw(
            "[1:7] unexpected symbol '=' near '0'"
        );
    });
    it("Throw unexpected boolean", () => {
        expect(testParse("true = 0")).to.throw(
            "[1:0] unexpected boolean 'true' near '='"
        );
    });
    it("Throw unexpected identifier", () => {
        expect(testParse("a, b() c()")).to.throw(
            "[1:7] unexpected identifier 'c' near '('"
        );
    });
});
