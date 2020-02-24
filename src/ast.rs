use crate::span::{FromInnerAndSpan, Span};

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum ExternalDeclaration {
    FunctionDefinition(FunctionDefinition),
    Declaration(DeclarationWithSpan),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct FunctionDefinitionWithSpan {
    pub inner: FunctionDefinition,
    pub span: Span,
}

impl FromInnerAndSpan for FunctionDefinitionWithSpan {
    type Inner = FunctionDefinition;

    fn from_inner_and_span(inner: FunctionDefinition, span: Span) -> FunctionDefinitionWithSpan {
        FunctionDefinitionWithSpan { inner, span }
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct FunctionDefinition {
    pub prototype: FunctionPrototypeWithSpan,
    pub body: CompoundStatementWithSpan,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct FunctionPrototypeWithSpan {
    pub inner: FunctionPrototype,
    pub span: Span,
}

impl FromInnerAndSpan for FunctionPrototypeWithSpan {
    type Inner = FunctionPrototype;

    fn from_inner_and_span(inner: FunctionPrototype, span: Span) -> FunctionPrototypeWithSpan {
        FunctionPrototypeWithSpan { inner, span }
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct FunctionPrototype {
    pub return_type: FullySpecifiedTypeWithSpan,
    pub name: IdentifierWithSpan,
    pub parameters: Vec<ParameterDeclarationWithSpan>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct DeclarationWithSpan {
    pub inner: Declaration,
    pub span: Span,
}

impl FromInnerAndSpan for DeclarationWithSpan {
    type Inner = Declaration;

    fn from_inner_and_span(inner: Declaration, span: Span) -> DeclarationWithSpan {
        DeclarationWithSpan { inner, span }
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Declaration {
    FunctionPrototype(FunctionPrototypeWithSpan),
    Variable {
        type_: FullySpecifiedTypeWithSpan,
        init_declarators: Vec<InitDeclarator>,
    },
    Precision {
        precision: PrecisionQualifierWithSpan,
        type_: TypeSpecifierWithSpan,
    },
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct FullySpecifiedTypeWithSpan {
    pub inner: FullySpecifiedType,
    pub span: Span,
}

impl FromInnerAndSpan for FullySpecifiedTypeWithSpan {
    type Inner = FullySpecifiedType;

    fn from_inner_and_span(inner: FullySpecifiedType, span: Span) -> FullySpecifiedTypeWithSpan {
        FullySpecifiedTypeWithSpan { inner, span }
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct FullySpecifiedType {
    pub qualifier: Option<TypeQualifierWithSpan>,
    pub type_: TypeSpecifierWithSpan,
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct TypeQualifierWithSpan {
    pub inner: TypeQualifier,
    pub span: Span,
}

impl FromInnerAndSpan for TypeQualifierWithSpan {
    type Inner = TypeQualifier;

    fn from_inner_and_span(inner: TypeQualifier, span: Span) -> TypeQualifierWithSpan {
        TypeQualifierWithSpan { inner, span }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum TypeQualifier {
    Const,
    Attribute,
    Uniform,
    Varying,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct ParameterDeclarationWithSpan {
    pub inner: ParameterDeclaration,
    pub span: Span,
}

impl FromInnerAndSpan for ParameterDeclarationWithSpan {
    type Inner = ParameterDeclaration;

    fn from_inner_and_span(
        inner: ParameterDeclaration,
        span: Span,
    ) -> ParameterDeclarationWithSpan {
        ParameterDeclarationWithSpan { inner, span }
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct ParameterDeclaration {
    pub qualifier: Option<ParameterQualifierWithSpan>,
    pub type_: TypeSpecifierWithSpan,
    pub declarator: Option<DeclaratorWithSpan>,
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ParameterQualifierWithSpan {
    pub inner: ParameterQualifier,
    pub span: Span,
}

impl FromInnerAndSpan for ParameterQualifierWithSpan {
    type Inner = ParameterQualifier;

    fn from_inner_and_span(inner: ParameterQualifier, span: Span) -> ParameterQualifierWithSpan {
        ParameterQualifierWithSpan { inner, span }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum ParameterQualifier {
    In,
    Out,
    Inout,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct TypeSpecifierWithSpan {
    pub inner: TypeSpecifier,
    pub span: Span,
}

impl FromInnerAndSpan for TypeSpecifierWithSpan {
    type Inner = TypeSpecifier;

    fn from_inner_and_span(inner: TypeSpecifier, span: Span) -> TypeSpecifierWithSpan {
        TypeSpecifierWithSpan { inner, span }
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct TypeSpecifier {
    pub precision: Option<PrecisionQualifierWithSpan>,
    pub no_precision: TypeSpecifierNoPrecision,
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct PrecisionQualifierWithSpan {
    pub inner: PrecisionQualifier,
    pub span: Span,
}

impl FromInnerAndSpan for PrecisionQualifierWithSpan {
    type Inner = PrecisionQualifier;

    fn from_inner_and_span(inner: PrecisionQualifier, span: Span) -> PrecisionQualifierWithSpan {
        PrecisionQualifierWithSpan { inner, span }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum PrecisionQualifier {
    Low,
    Medium,
    High,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct TypeSpecifierNoPrecisionWithSpan {
    pub inner: TypeSpecifierNoPrecision,
    pub span: Span,
}

impl FromInnerAndSpan for TypeSpecifierNoPrecisionWithSpan {
    type Inner = TypeSpecifierNoPrecision;

    fn from_inner_and_span(
        inner: TypeSpecifierNoPrecision,
        span: Span,
    ) -> TypeSpecifierNoPrecisionWithSpan {
        TypeSpecifierNoPrecisionWithSpan { inner, span }
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum TypeSpecifierNoPrecision {
    Struct {
        name: Option<IdentifierWithSpan>,
        declarations: Vec<StructDeclaration>,
    },
    TypeIdentifier(TypeIdentifierWithSpan),
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct TypeIdentifierWithSpan {
    pub inner: TypeIdentifier,
    pub span: Span,
}

impl FromInnerAndSpan for TypeIdentifierWithSpan {
    type Inner = TypeIdentifier;

    fn from_inner_and_span(inner: TypeIdentifier, span: Span) -> TypeIdentifierWithSpan {
        TypeIdentifierWithSpan { inner, span }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum TypeIdentifier {
    Void,
    Float,
    Bool,
    Int,
    Vec2,
    Vec3,
    Vec4,
    Bvec2,
    Bvec3,
    Bvec4,
    Ivec2,
    Ivec3,
    Ivec4,
    Mat2,
    Mat3,
    Mat4,
    Sampler2D,
    SamplerCube,
    Identifier,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct StructDeclarationWithSpan {
    pub inner: StructDeclaration,
    pub span: Span,
}

impl FromInnerAndSpan for StructDeclarationWithSpan {
    type Inner = StructDeclaration;

    fn from_inner_and_span(inner: StructDeclaration, span: Span) -> StructDeclarationWithSpan {
        StructDeclarationWithSpan { inner, span }
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct StructDeclaration {
    pub type_: TypeSpecifierWithSpan,
    pub declarators: Vec<DeclaratorWithSpan>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum InitDeclarator {
    Declarator(DeclaratorWithSpan),
    NameInitializer(NameInitializerWithSpan),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct DeclaratorWithSpan {
    pub inner: Declarator,
    pub span: Span,
}

impl FromInnerAndSpan for DeclaratorWithSpan {
    type Inner = Declarator;

    fn from_inner_and_span(inner: Declarator, span: Span) -> DeclaratorWithSpan {
        DeclaratorWithSpan { inner, span }
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Declarator {
    pub name: IdentifierWithSpan,
    pub length: Option<ExpressionWithSpan>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct NameInitializerWithSpan {
    pub inner: NameInitializer,
    pub span: Span,
}

impl FromInnerAndSpan for NameInitializerWithSpan {
    type Inner = NameInitializer;

    fn from_inner_and_span(inner: NameInitializer, span: Span) -> NameInitializerWithSpan {
        NameInitializerWithSpan { inner, span }
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct NameInitializer {
    pub name: IdentifierWithSpan,
    pub initializer: ExpressionWithSpan,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct StatementWithSpan {
    pub inner: Statement,
    pub span: Span,
}

impl FromInnerAndSpan for StatementWithSpan {
    type Inner = Statement;

    fn from_inner_and_span(inner: Statement, span: Span) -> StatementWithSpan {
        StatementWithSpan { inner, span }
    }
}

impl From<CompoundStatementWithSpan> for StatementWithSpan {
    fn from(compound_statement: CompoundStatementWithSpan) -> StatementWithSpan {
        let span = compound_statement.span;
        StatementWithSpan {
            inner: Statement::Compound(compound_statement),
            span,
        }
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Statement {
    Compound(CompoundStatementWithSpan),
    DeclarationOrExpression(DeclarationOrExpressionStatement),
    If {
        condition: ExpressionWithSpan,
        consequent: Box<StatementWithSpan>,
        alternative: Option<Box<StatementWithSpan>>,
    },
    While {
        condition: Condition,
        body: Box<StatementWithSpan>,
    },
    DoWhile {
        body: Box<StatementWithSpan>,
        condition: ExpressionWithSpan,
    },
    For {
        initialization: DeclarationOrExpressionStatement,
        condition: Option<Condition>,
        afterthought: Option<ExpressionWithSpan>,
        body: Box<StatementWithSpan>,
    },
    ForIn {
        name: IdentifierWithSpan,
        start: ExpressionWithSpan,
        end: ExpressionWithSpan,
        body: Box<CompoundStatementWithSpan>,
    },
    Continue,
    Break,
    Return {
        value: Option<ExpressionWithSpan>,
    },
    Discard,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct CompoundStatementWithSpan {
    inner: CompoundStatement,
    span: Span,
}

impl FromInnerAndSpan for CompoundStatementWithSpan {
    type Inner = CompoundStatement;

    fn from_inner_and_span(inner: CompoundStatement, span: Span) -> CompoundStatementWithSpan {
        CompoundStatementWithSpan { inner, span }
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct CompoundStatement {
    pub statements: Vec<StatementWithSpan>
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum DeclarationOrExpressionStatement {
    Declaration(DeclarationWithSpan),
    ExpressionStatement(ExpressionStatementWithSpan),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct ExpressionStatementWithSpan {
    inner: ExpressionStatement,
    span: Span,
}

impl FromInnerAndSpan for ExpressionStatementWithSpan {
    type Inner = ExpressionStatement;

    fn from_inner_and_span(inner: ExpressionStatement, span: Span) -> ExpressionStatementWithSpan {
        ExpressionStatementWithSpan { inner, span }
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct ExpressionStatement {
    pub expression: Option<ExpressionWithSpan>
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Condition {
    Expression(ExpressionWithSpan),
    NameInitializer(NameInitializerWithSpan),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct ExpressionWithSpan {
    pub inner: Expression,
    pub span: Span,
}

impl FromInnerAndSpan for ExpressionWithSpan {
    type Inner = Expression;

    fn from_inner_and_span(inner: Expression, span: Span) -> ExpressionWithSpan {
        ExpressionWithSpan { inner, span }
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Expression {
    Sequence {
        expressions: Vec<ExpressionWithSpan>,
    },
    Conditional {
        condition: Box<ExpressionWithSpan>,
        consequent: Box<ExpressionWithSpan>,
        alternative: Box<ExpressionWithSpan>,
    },
    Binary {
        operator: BinaryOperatorWithSpan,
        left_operand: Box<ExpressionWithSpan>,
        right_operand: Box<ExpressionWithSpan>,
    },
    Unary {
        operator: UnaryOperatorWithSpan,
        operand: Box<ExpressionWithSpan>,
    },
    Field {
        expression: Box<ExpressionWithSpan>,
        name: IdentifierWithSpan,
    },
    Element {
        expression: Box<ExpressionWithSpan>,
        index: Box<ExpressionWithSpan>,
    },
    Call {
        name: FunctionIdentifierWithSpan,
        arguments: Vec<ExpressionWithSpan>,
    },
    BoolConstant(bool),
    IntConstant(i32),
    FloatConstant(f32),
    Identifier,
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct BinaryOperatorWithSpan {
    pub inner: BinaryOperator,
    pub span: Span,
}

impl FromInnerAndSpan for BinaryOperatorWithSpan {
    type Inner = BinaryOperator;

    fn from_inner_and_span(inner: BinaryOperator, span: Span) -> BinaryOperatorWithSpan {
        BinaryOperatorWithSpan { inner, span }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum BinaryOperator {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    LogicalOr,
    LogicalXor,
    LogicalAnd,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct UnaryOperatorWithSpan {
    pub inner: UnaryOperator,
    pub span: Span,
}

impl FromInnerAndSpan for UnaryOperatorWithSpan {
    type Inner = UnaryOperator;

    fn from_inner_and_span(inner: UnaryOperator, span: Span) -> UnaryOperatorWithSpan {
        UnaryOperatorWithSpan { inner, span }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum UnaryOperator {
    Not,
    Plus,
    PreInc,
    Minus,
    PreDec,
    PostInc,
    PostDec,
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct FunctionIdentifierWithSpan {
    pub inner: FunctionIdentifier,
    pub span: Span,
}

impl FromInnerAndSpan for FunctionIdentifierWithSpan {
    type Inner = FunctionIdentifier;

    fn from_inner_and_span(inner: FunctionIdentifier, span: Span) -> FunctionIdentifierWithSpan {
        FunctionIdentifierWithSpan { inner, span }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum FunctionIdentifier {
    Float,
    Bool,
    Int,
    Vec2,
    Vec3,
    Vec4,
    Bvec2,
    Bvec3,
    Bvec4,
    Ivec2,
    Ivec3,
    Ivec4,
    Mat2,
    Mat3,
    Mat4,
    Identifier,
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct IdentifierWithSpan {
    pub span: Span,
}

impl FromInnerAndSpan for IdentifierWithSpan {
    type Inner = Identifier;

    fn from_inner_and_span(_inner: Identifier, span: Span) -> IdentifierWithSpan {
        IdentifierWithSpan { span }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Identifier;
