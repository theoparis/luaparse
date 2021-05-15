export type BinaryOperator =
    | "+"
    | "-"
    | "*"
    | "/"
    | "%"
    | "^"
    | "-"
    | "=="
    | "~="
    | ">"
    | "<"
    | ">="
    | "<="
    | "..";
export type LogicalOperator = "and" | "or";
export type UnaryOperator = "not" | "-" | "~" | "#";
export type Indexer = "." | ":";

export type TextRange = [number, number];

export interface Location {
    start: {
        line: number;
        column: number;
    };
    end: {
        line: number;
        column: number;
    };
}

export interface NodeAdditional {
    type: string;
    range?: TextRange;
    loc?: Location;
    raw?: string;
}

export type Block = Statement[];

export interface LabelStatement extends NodeAdditional {
    type: "LabelStatement";
    label: Identifier;
}

export interface BreakStatement extends NodeAdditional {
    type: "BreakStatement";
}

export interface GotoStatement extends NodeAdditional {
    type: "GotoStatement";
    label: Identifier;
}

export interface ReturnStatement extends NodeAdditional {
    type: "ReturnStatement";
    arguments: Expression[];
}

export interface IfStatement extends NodeAdditional {
    type: "IfStatement";
    clauses: [IfClause, ...Array<ElseifClause | ElseClause>];
}

export interface IfClause extends NodeAdditional {
    type: "IfClause";
    condition: Expression;
    body: Block;
}

export interface ElseifClause extends NodeAdditional {
    type: "ElseifClause";
    condition: Expression;
    body: Block;
}

export interface ElseClause extends NodeAdditional {
    type: "ElseClause";
    body: Block;
}

export interface WhileStatement extends NodeAdditional {
    type: "WhileStatement";
    condition: Expression;
    body: Block;
}

export interface DoStatement extends NodeAdditional {
    type: "DoStatement";
    body: Block;
}

export interface RepeatStatement extends NodeAdditional {
    type: "RepeatStatement";
    condition: Expression;
    body: Block;
}

export interface LocalStatement extends NodeAdditional {
    type: "LocalStatement";
    variables: Identifier[];
    init: Expression[];
}

export interface AssignmentStatement extends NodeAdditional {
    type: "AssignmentStatement";
    variables: Array<Identifier | MemberExpression | IndexExpression>;
    init: Expression[];
}

export interface CallStatement extends NodeAdditional {
    type: "CallStatement";
    expression: CallExpression;
}

export interface FunctionDeclaration extends NodeAdditional {
    type: "FunctionDeclaration";
    identifier: Identifier | MemberExpression;
    isLocal: boolean;
    parameters: Array<Identifier | VarargLiteral>;
    body: Block;
}

export interface FunctionExpression extends NodeAdditional {
    type: "FunctionDeclaration";
    identifier: null;
    isLocal: false;
    parameters: FunctionDeclaration["parameters"];
    body: Block;
}

export interface ForNumericStatement extends NodeAdditional {
    type: "ForNumericStatement";
    variable: Identifier;
    start: Expression;
    end: Expression;
    step: Expression | null;
    body: Block;
}

export interface ForGenericStatement extends NodeAdditional {
    type: "ForGenericStatement";
    variables: Identifier[];
    iterators: Expression[];
    body: Block;
}

export interface Chunk extends NodeAdditional {
    type: "Chunk";
    body: Block;
    comments?: Comment[];
    globals?: Identifier[];
}

export interface Identifier extends NodeAdditional {
    type: "Identifier";
    name: string;
    isLocal?: boolean;
}

export interface NumericLiteral extends NodeAdditional {
    type: "NumericLiteral";
    value: number;
}

export interface StringLiteral extends NodeAdditional {
    type: "StringLiteral";
    value: string;
    raw: string;
}

export interface BooleanLiteral extends NodeAdditional {
    type: "BooleanLiteral";
    value: boolean;
}

export interface VarargLiteral extends NodeAdditional {
    type: "VarargLiteral";
    value: "...";
}

export interface NilLiteral extends NodeAdditional {
    type: "NilLiteral";
    value: null;
    raw: "nil";
}

export interface TableKey extends NodeAdditional {
    type: "TableKey";
    key: Expression;
    value: Expression;
}

export interface TableKeyString extends NodeAdditional {
    type: "TableKeyString";
    key: Identifier;
    value: Expression;
}

export interface TableValue extends NodeAdditional {
    type: "TableValue";
    value: Expression;
}

export interface TableConstructorExpression extends NodeAdditional {
    type: "TableConstructorExpression";
    fields: Array<TableKey | TableKeyString | TableValue>;
}

export interface BinaryExpression extends NodeAdditional {
    type: "BinaryExpression";
    operator: BinaryOperator;
    left: Expression;
    right: Expression;
}

export interface LogicalExpression extends NodeAdditional {
    type: "LogicalExpression";
    operator: LogicalOperator;
    left: Expression;
    right: Expression;
}

export interface UnaryExpression extends NodeAdditional {
    type: "UnaryExpression";
    operator: UnaryOperator;
    argument: Expression;
}

export interface MemberExpression extends NodeAdditional {
    type: "MemberExpression";
    indexer: Indexer;
    identifier: Identifier;
    base: Identifier | Expression;
}

export interface IndexExpression extends NodeAdditional {
    type: "IndexExpression";
    base: Expression /* Identifier | MemberExpression */;
    index: Expression;
}

export interface CallExpression extends NodeAdditional {
    type: "CallExpression";
    base: Identifier | MemberExpression;
    arguments: Expression[];
}

export interface TableCallExpression extends NodeAdditional {
    type: "TableCallExpression";
    base: Identifier | MemberExpression;
    arguments: TableConstructorExpression;
}

export interface StringCallExpression extends NodeAdditional {
    type: "StringCallExpression";
    base: Identifier | MemberExpression;
    argument: StringLiteral;
}

export interface Comment extends NodeAdditional {
    type: "Comment";
    value: string;
}

export type Statement =
    | LabelStatement
    | BreakStatement
    | GotoStatement
    | ReturnStatement
    | IfStatement
    | WhileStatement
    | DoStatement
    | RepeatStatement
    | LocalStatement
    | AssignmentStatement
    | CallStatement
    | FunctionDeclaration
    | ForNumericStatement
    | ForGenericStatement;

export type Literal =
    | NumericLiteral
    | StringLiteral
    | BooleanLiteral
    | VarargLiteral
    | NilLiteral;

export type Expression =
    | Identifier
    | Literal
    | TableConstructorExpression
    | BinaryExpression
    | LogicalExpression
    | UnaryExpression
    | MemberExpression
    | IndexExpression
    | CallExpression
    | TableCallExpression
    | StringCallExpression
    | FunctionExpression;

export type PrimaryExpression =
    | Literal
    | TableConstructorExpression
    | FunctionExpression;

export type IfClauses = [IfClause, ...Array<ElseifClause | ElseClause>];
export type Clauses = ElseifClause | IfClause | ElseifClause | ElseClause;

export type Node =
    | Chunk
    | Expression
    | Statement
    | TableKey
    | TableKeyString
    | TableValue
    | Comment
    | Clauses;

export interface LuaAstBuilder {
    labelStatement: (label: Identifier) => LabelStatement;
    breakStatement: () => BreakStatement;
    gotoStatement: (label: Identifier) => GotoStatement;
    returnStatement: (args: Expression[]) => ReturnStatement;
    ifStatement: (clauses: IfClauses) => IfStatement;
    ifClause: (condition: Expression, body: Block) => IfClause;
    elseifClause: (condition: Expression, body: Block) => ElseifClause;
    elseClause: (body: Block) => ElseClause;
    whileStatement: (condition: Expression, body: Block) => WhileStatement;
    doStatement: (body: Block) => DoStatement;
    repeatStatement: (condition: Expression, body: Block) => RepeatStatement;
    localStatement: (
        variables: Identifier[],
        init: Expression[]
    ) => LocalStatement;
    assignmentStatement: (
        variables: Array<Identifier | MemberExpression | IndexExpression>,
        init: Expression[]
    ) => AssignmentStatement;
    callStatement: (expression: CallExpression) => CallStatement;
    functionStatement: (
        identifier: Identifier | MemberExpression | null,
        parameters: Array<Identifier | VarargLiteral>,
        isLocal: boolean,
        body: Block
    ) => FunctionDeclaration | FunctionExpression;
    forNumericStatement: (
        variable: Identifier,
        start: Expression,
        end: Expression,
        step: Expression | null,
        body: Block
    ) => ForNumericStatement;
    forGenericStatement: (
        variables: Identifier[],
        iterators: Expression[],
        body: Block
    ) => ForGenericStatement;
    chunk: (body: Block) => Chunk;
    identifier: (name: string) => Identifier;
    literal: (
        type: number,
        value: string | number | boolean | null | "...",
        raw: string
    ) => Literal;
    tableKey: (key: Expression, value: Expression) => TableKey;
    tableKeyString: (key: Identifier, value: Expression) => TableKeyString;
    tableValue: (value: Expression) => TableValue;
    tableConstructorExpression: (
        fields: Array<TableKey | TableKeyString | TableValue>
    ) => TableConstructorExpression;
    binaryExpression: (
        operator: BinaryOperator | LogicalOperator,
        left: Expression,
        right: Expression
    ) => BinaryExpression | LogicalExpression;
    unaryExpression: (
        operator: UnaryOperator,
        argument: Expression
    ) => UnaryExpression;
    memberExpression: (
        base: Identifier | Expression,
        indexer: Indexer,
        identifier: Identifier
    ) => MemberExpression;
    indexExpression: (base: Expression, index: Expression) => IndexExpression;
    callExpression: (
        base: Identifier | MemberExpression,
        args: Expression[]
    ) => CallExpression;
    tableCallExpression: (
        base: Identifier | MemberExpression,
        args: TableConstructorExpression
    ) => TableCallExpression;
    stringCallExpression: (
        base: Identifier | MemberExpression,
        argument: StringLiteral
    ) => StringCallExpression;
    comment: (value: string, raw: string) => Comment;
}
