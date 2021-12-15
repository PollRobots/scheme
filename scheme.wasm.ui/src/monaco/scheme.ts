import { Monaco } from "@monaco-editor/react";
import { languages } from "monaco-editor";

const kBuiltins = [
  "+",
  "-",
  "*",
  "integer?",
  "=",
  "<",
  "<=",
  ">",
  ">=",
  "abs",
  "quotient",
  "remainder",
  "truncate-quotient",
  "truncate-remainder",
  "floor-quotient",
  "floor-remainder",
  "floor/",
  "truncate/",
  "exact-integer-sqrt",
  "string->number",
  "number->string",
  "not",
  "boolean?",
  "boolean=?",
  "pair?",
  "cons",
  "car",
  "cdr",
  "set-car!",
  "set-cdr!",
  "null?",
  "list?",
  "make-list",
  "list",
  "length",
  "eq?",
  "eqv?",
  "equal?",
  "append",
  "reverse",
  "list-tail",
  "list-ref",
  "list-set!",
  "memq",
  "memv",
  "member",
  "assq",
  "assv",
  "assoc",
  "list-copy",
  "symbol?",
  "symbol=?",
  "symbol->string",
  "symbol->string",
  "exit",
  "char?",
  "char=?",
  "char<?",
  "char<=?",
  "char>?",
  "char>=?",
  "char-alphabetic?",
  "char-numeric?",
  "char-whitespace?",
  "char-upper-case?",
  "char-lower-case?",
  "digit-value",
  "char->integer",
  "integer->char",
  "char-upcase",
  "char-downcase",
  "char-foldcase",
  "char-ci=?",
  "char-ci<?",
  "char-ci<=?",
  "char-ci>?",
  "char-ci>=?",
  "string?",
  "make-string",
  "string",
  "string-length",
  "string-ref",
  "string-set!",
  "string=?",
  "string-ci=?",
  "string>?",
  "string-ci>?",
  "string>=?",
  "string-ci>=?",
  "string<?",
  "string-ci<?",
  "string<=?",
  "string-ci<=?",
  "string-upcase",
  "string-downcase",
  "string-foldcase",
  "substring",
  "string-copy",
  "string->list",
  "list->string",
  "string-append",
  "string-copy!",
  "string-fill!",
  "vector?",
  "make-vector",
  "vector",
  "vector-length",
  "vector-ref",
  "vector-set!",
  "vector-copy",
  "vector-copy!",
  "vector->string",
  "string->vector",
  "vector->list",
  "list->vector",
  "vector-append",
  "vector-fill!",
  "values",
  "exact?",
  "inexact?",
  "inexact",
];

const kSpecial = [
  "if",
  "let",
  "let*",
  "letrec",
  "letrec*",
  "let-values",
  "let*-values",
  "lambda",
  "define",
  "quote",
  "set!",
  "cond",
  "case",
  "and",
  "or",
  "when",
  "unless",
  "begin",
];
const kConf: languages.LanguageConfiguration = {
  comments: {
    lineComment: ";",
    blockComment: ["#|", "|#"],
  },

  brackets: [
    ["(", ")"],
    ["#(", ")"],
    ["#u8(", ")"],
  ],

  autoClosingPairs: [
    { open: "(", close: ")" },
    { open: "#(", close: ")" },
    { open: "#u8(", close: ")" },
    { open: '"', close: '"' },
  ],
};

const kLanguage: languages.IMonarchLanguage = {
  defaultToken: "",
  ignoreCase: false,
  tokenPostfix: ".scheme",

  brackets: [
    { open: "#(", close: ")", token: "delimiter.vector" },
    { open: "#u8(", close: ")", token: "delimiter.bytevector" },
    { open: "(", close: ")", token: "delimiter.parenthesis" },
  ],

  keywords: kBuiltins,

  constants: ["#t", "#f"],

  operators: kSpecial,

  tokenizer: {
    root: [
      [/(?:#[iIeE])?#[xX][+-]?[0-9a-fA-F]+/, "number.hex"],
      [/(?:#[iIeE])?#[oO][+-]?[0-7]+/, "number.oct"],
      [/(?:#[iIeE])?#[bB][+-]?[01]+/, "number.bin"],
      [/(?:#[iIeE])?#[dD][+-]?[0-9]+/, "number.dec"],

      [/[+-]?\d+(?:(?:\.\d*)?(?:[eE][+-]?\d+)?)?/, "number.float"],
      [/[+-](inf|nan)\.0/, "number.float"],

      [
        /#\\(alarm|backspace|delete|escape|newline|null|return|space|tab)/,
        "string.character",
      ],
      [/#\\x[0-9a-fA-F]+/, "string.character"],
      [/#\\\w/, "string"],

      [
        /(?:\b(?:(define|define-syntax|define-macro))\b)(\s+)((?:\w|\-|\!|\?)*)/,
        ["keyword", "white", "variable"],
      ],

      { include: "@whitespace" },
      { include: "@strings" },
      { include: "@brackets" },

      [
        /[a-zA-Z_#][a-zA-Z0-9_\-\?\!\*]*/,
        {
          cases: {
            "@keywords": "keyword",
            "@constants": "constant",
            "@operators": "operators",
            "@default": "identifier",
          },
        },
      ],
    ],

    brackets: [
      [/[#](?:u8)?(?=\()/, "delimiter.vector"],
      [/\(/, "delimiter.open"],
      [/\)/, "delimiter.close"],
    ],

    comment: [
      [/[^\|#]+/, "comment"],
      [/#\|/, "comment", "@push"],
      [/\|#/, "comment", "@pop"],
      [/[\|#]/, "comment"],
    ],

    whitespace: [
      [/[ \t\r\n]+/, "white"],
      [/#\|/, "comment", "@comment"],
      [/;.*$/, "comment"],
    ],

    strings: [
      [/"$/, "string", "@popall"],
      [/"(?=.)/, "string", "@multiLineString"],
    ],

    multiLineString: [
      [/[^\\"]+$/, "string", "@popall"],
      [/[^\\"]+/, "string"],
      [/\\./, "string.escape"],
      [/"/, "string", "@popall"],
      [/\\$/, "string"],
    ],
  },
};

export const kLanguageId = "r7rsScheme";

export function registerLanguage(monaco: Monaco) {
  monaco.languages.register({ id: kLanguageId });
  monaco.languages.setMonarchTokensProvider(kLanguageId, kLanguage);
  monaco.languages.setLanguageConfiguration(kLanguageId, kConf);
}