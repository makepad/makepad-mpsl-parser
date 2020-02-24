use crate::ast::*;
use crate::span::{FromInnerAndSpan, Span};
use crate::token::{Token, TokenWithSpan};
use std::result;

pub trait TokenExt {
    fn to_assignment_operator(self) -> Option<BinaryOperator>;
    fn to_logical_or_operator(self) -> Option<BinaryOperator>;
    fn to_logical_xor_operator(self) -> Option<BinaryOperator>;
    fn to_logical_and_operator(self) -> Option<BinaryOperator>;
    fn to_equality_operator(self) -> Option<BinaryOperator>;
    fn to_relational_operator(self) -> Option<BinaryOperator>;
    fn to_additive_operator(self) -> Option<BinaryOperator>;
    fn to_multiplicative_operator(self) -> Option<BinaryOperator>;
    fn to_unary_operator(self) -> Option<UnaryOperator>;
    fn to_postfix_operator(self) -> Option<UnaryOperator>;
    fn to_type_qualifier(self) -> Option<TypeQualifier>;
    fn to_parameter_qualifier(self) -> Option<ParameterQualifier>;
    fn to_precision_qualifier(self) -> Option<PrecisionQualifier>;
    fn to_type_identifier(self) -> Option<TypeIdentifier>;
    fn to_function_identifier(self) -> Option<FunctionIdentifier>;
    fn to_identifier(self) -> Option<Identifier>;
}

impl TokenExt for Token {
    fn to_assignment_operator(self) -> Option<BinaryOperator> {
        match self {
            Token::Eq => Some(BinaryOperator::Assign),
            Token::PlusEq => Some(BinaryOperator::AddAssign),
            Token::MinusEq => Some(BinaryOperator::SubAssign),
            Token::StarEq => Some(BinaryOperator::MulAssign),
            Token::SlashEq => Some(BinaryOperator::DivAssign),
            _ => None,
        }
    }

    fn to_logical_or_operator(self) -> Option<BinaryOperator> {
        if let Token::OrOr = self {
            Some(BinaryOperator::LogicalOr)
        } else {
            None
        }
    }

    fn to_logical_xor_operator(self) -> Option<BinaryOperator> {
        if let Token::XorXor = self {
            Some(BinaryOperator::LogicalXor)
        } else {
            None
        }
    }

    fn to_logical_and_operator(self) -> Option<BinaryOperator> {
        if let Token::AndAnd = self {
            Some(BinaryOperator::LogicalAnd)
        } else {
            None
        }
    }

    fn to_equality_operator(self) -> Option<BinaryOperator> {
        match self {
            Token::EqEq => Some(BinaryOperator::Eq),
            Token::Ne => Some(BinaryOperator::Ne),
            _ => None,
        }
    }

    fn to_relational_operator(self) -> Option<BinaryOperator> {
        match self {
            Token::Lt => Some(BinaryOperator::Lt),
            Token::Le => Some(BinaryOperator::Le),
            Token::Gt => Some(BinaryOperator::Gt),
            Token::Ge => Some(BinaryOperator::Ge),
            _ => None,
        }
    }

    fn to_additive_operator(self) -> Option<BinaryOperator> {
        match self {
            Token::Plus => Some(BinaryOperator::Add),
            Token::Minus => Some(BinaryOperator::Sub),
            _ => None,
        }
    }

    fn to_multiplicative_operator(self) -> Option<BinaryOperator> {
        match self {
            Token::Star => Some(BinaryOperator::Mul),
            Token::Slash => Some(BinaryOperator::Div),
            _ => None,
        }
    }

    fn to_unary_operator(self) -> Option<UnaryOperator> {
        match self {
            Token::Not => Some(UnaryOperator::Not),
            Token::Plus => Some(UnaryOperator::Plus),
            Token::PlusPlus => Some(UnaryOperator::PreInc),
            Token::Minus => Some(UnaryOperator::Minus),
            Token::MinusMinus => Some(UnaryOperator::PreDec),
            _ => None,
        }
    }

    fn to_postfix_operator(self) -> Option<UnaryOperator> {
        match self {
            Token::PlusPlus => Some(UnaryOperator::PostInc),
            Token::MinusMinus => Some(UnaryOperator::PostDec),
            _ => None,
        }
    }

    fn to_type_qualifier(self) -> Option<TypeQualifier> {
        match self {
            Token::Const => Some(TypeQualifier::Const),
            Token::Attribute => Some(TypeQualifier::Attribute),
            Token::Uniform => Some(TypeQualifier::Uniform),
            Token::Varying => Some(TypeQualifier::Varying),
            _ => None,
        }
    }

    fn to_parameter_qualifier(self) -> Option<ParameterQualifier> {
        match self {
            Token::In => Some(ParameterQualifier::In),
            Token::Out => Some(ParameterQualifier::Out),
            Token::Inout => Some(ParameterQualifier::Inout),
            _ => None,
        }
    }

    fn to_precision_qualifier(self) -> Option<PrecisionQualifier> {
        match self {
            Token::Lowp => Some(PrecisionQualifier::Low),
            Token::Mediump => Some(PrecisionQualifier::Medium),
            Token::Highp => Some(PrecisionQualifier::High),
            _ => None,
        }
    }

    fn to_type_identifier(self) -> Option<TypeIdentifier> {
        match self {
            Token::Void => Some(TypeIdentifier::Void),
            Token::Float => Some(TypeIdentifier::Float),
            Token::Int => Some(TypeIdentifier::Int),
            Token::Bool => Some(TypeIdentifier::Bool),
            Token::Vec2 => Some(TypeIdentifier::Vec2),
            Token::Vec3 => Some(TypeIdentifier::Vec3),
            Token::Vec4 => Some(TypeIdentifier::Vec4),
            Token::Bvec2 => Some(TypeIdentifier::Bvec2),
            Token::Bvec3 => Some(TypeIdentifier::Bvec3),
            Token::Bvec4 => Some(TypeIdentifier::Bvec4),
            Token::Ivec2 => Some(TypeIdentifier::Ivec2),
            Token::Ivec3 => Some(TypeIdentifier::Ivec3),
            Token::Ivec4 => Some(TypeIdentifier::Ivec4),
            Token::Mat2 => Some(TypeIdentifier::Mat2),
            Token::Mat3 => Some(TypeIdentifier::Mat3),
            Token::Mat4 => Some(TypeIdentifier::Mat4),
            Token::Sampler2D => Some(TypeIdentifier::Sampler2D),
            Token::SamplerCube => Some(TypeIdentifier::SamplerCube),
            _ => None,
        }
    }

    fn to_function_identifier(self) -> Option<FunctionIdentifier> {
        match self {
            Token::Float => Some(FunctionIdentifier::Float),
            Token::Int => Some(FunctionIdentifier::Int),
            Token::Bool => Some(FunctionIdentifier::Bool),
            Token::Vec2 => Some(FunctionIdentifier::Vec2),
            Token::Vec3 => Some(FunctionIdentifier::Vec3),
            Token::Vec4 => Some(FunctionIdentifier::Vec4),
            Token::Bvec2 => Some(FunctionIdentifier::Bvec2),
            Token::Bvec3 => Some(FunctionIdentifier::Bvec3),
            Token::Bvec4 => Some(FunctionIdentifier::Bvec4),
            Token::Ivec2 => Some(FunctionIdentifier::Ivec2),
            Token::Ivec3 => Some(FunctionIdentifier::Ivec3),
            Token::Ivec4 => Some(FunctionIdentifier::Ivec4),
            Token::Mat2 => Some(FunctionIdentifier::Mat2),
            Token::Mat3 => Some(FunctionIdentifier::Mat3),
            Token::Mat4 => Some(FunctionIdentifier::Mat4),
            Token::Identifier => Some(FunctionIdentifier::Identifier),
            _ => None,
        }
    }

    fn to_identifier(self) -> Option<Identifier> {
        if let Token::Identifier = self {
            Some(Identifier)
        } else {
            None
        }
    }
}

#[derive(Clone, Debug)]
pub struct Parser<'a> {
    tokens: &'a [TokenWithSpan],
    index: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &[TokenWithSpan]) -> Parser {
        Parser { tokens, index: 0 }
    }

    pub fn parse(&mut self) -> Result<Vec<ExternalDeclaration>> {
        let mut declarations = vec![];
        while self.peek_token() != Token::Eof {
            declarations.push(self.parse_external_declaration()?);
        }
        Ok(declarations)
    }

    fn parse_external_declaration(&mut self) -> Result<ExternalDeclaration> {
        self.parse_alternatives(&mut [
            &mut |this| {
                Ok(ExternalDeclaration::FunctionDefinition(
                    this.parse_function_definition()?,
                ))
            },
            &mut |this| Ok(ExternalDeclaration::Declaration(this.parse_declaration()?)),
        ])
    }

    fn parse_function_definition(&mut self) -> Result<FunctionDefinition> {
        let prototype = self.parse_function_prototype()?;
        let body = self.parse_compound_statement()?;
        Ok(FunctionDefinition { prototype, body })
    }

    fn parse_function_prototype(&mut self) -> Result<FunctionPrototypeWithSpan> {
        let span = self.begin_span();
        let return_type;
        let name;
        let mut parameters = vec![];
        if self.accept_token(Token::Fn) {
            name = self.expect_map_token(|token| token.to_identifier())?;
            self.expect_token(Token::LeftParen)?;
            if !self.accept_token(Token::RightParen) {
                loop {
                    let span = self.begin_span();
                    let qualifier = self.accept_map_token(|token| token.to_parameter_qualifier());
                    let name: IdentifierWithSpan = self.expect_map_token(|token| token.to_identifier())?;
                    self.expect_token(Token::Colon)?;
                    let type_ = self.parse_type_specifier()?;
                    parameters.push(span.end(self, ParameterDeclaration {
                        qualifier,
                        type_,
                        declarator: Some(name.into()),
                    }));
                    if !self.accept_token(Token::Comma) {
                        break;
                    }
                }
                self.expect_token(Token::RightParen)?;
            }
            return_type = if self.accept_token(Token::RightArrow) {
                Some(self.parse_fully_specified_type()?)
            } else {
                None
            }
        } else {
            return_type = Some(self.parse_fully_specified_type()?);
            name = self.expect_map_token(|token| token.to_identifier())?;
            self.expect_token(Token::LeftParen)?;
            if !self.accept_token(Token::RightParen) {
                loop {
                    parameters.push(self.parse_parameter_declaration()?);
                    if !self.accept_token(Token::Comma) {
                        break;
                    }
                }
                self.expect_token(Token::RightParen)?;
            }
        }
        Ok(span.end(
            self,
            FunctionPrototype {
                return_type,
                name,
                parameters,
            },
        ))
    }

    fn parse_declaration(&mut self) -> Result<DeclarationWithSpan> {
        self.parse_alternatives(&mut [
            &mut |this| {
                let span = this.begin_span();
                let prototype = this.parse_function_prototype()?;
                this.expect_token(Token::Semicolon)?;
                Ok(span.end(this, Declaration::Function { prototype }))
            },
            &mut |this| {
                let span = this.begin_span();
                match this.peek_token() {
                    Token::Let => {
                        this.skip_token();
                        let name = this.expect_map_token(|token| token.to_identifier())?;
                        let type_ = if this.accept_token(Token::Colon) {
                            Some(this.parse_fully_specified_type()?)
                        } else {
                            None
                        };
                        let initializer = if this.accept_token(Token::Eq) {
                            Some(this.parse_expression()?)
                        } else {
                            None
                        };
                        this.expect_token(Token::Semicolon)?;
                        Ok(span.end(this, Declaration::Let {
                            name,
                            type_,
                            initializer,
                        }))
                    }
                    Token::Precision => {
                        this.skip_token();
                        let precision = this.expect_map_token(|token| token.to_precision_qualifier())?;
                        let type_ = this.parse_type_specifier()?;
                        this.expect_token(Token::Semicolon)?;
                        Ok(span.end(this, Declaration::Precision { precision, type_ }))
                    }
                    _ => {
                        let type_ = this.parse_fully_specified_type()?;
                        let mut init_declarators = vec![];
                        if !this.accept_token(Token::Semicolon) {
                            loop {
                                init_declarators.push(this.parse_init_declarator()?);
                                if !this.accept_token(Token::Comma) {
                                    break;
                                }
                            }
                            this.expect_token(Token::Semicolon)?
                        }
                        Ok(span.end(
                            this,
                            Declaration::Variable {
                                type_,
                                init_declarators,
                            },
                        ))
                    }
                }
            },
        ])
    }

    fn parse_fully_specified_type(&mut self) -> Result<FullySpecifiedTypeWithSpan> {
        let span = self.begin_span();
        let qualifier = self.accept_map_token(|token| token.to_type_qualifier());
        let type_ = self.parse_type_specifier()?;
        Ok(span.end(self, FullySpecifiedType { qualifier, type_ }))
    }

    pub fn parse_parameter_declaration(&mut self) -> Result<ParameterDeclarationWithSpan> {
        let span = self.begin_span();
        let qualifier = self.accept_map_token(|token| token.to_parameter_qualifier());
        let type_ = self.parse_type_specifier()?;
        let declarator = if self.peek_token() == Token::Identifier {
            Some(self.parse_declarator()?)
        } else {
            None
        };
        Ok(span.end(
            self,
            ParameterDeclaration {
                qualifier,
                type_,
                declarator,
            },
        ))
    }

    fn parse_type_specifier(&mut self) -> Result<TypeSpecifierWithSpan> {
        let span = self.begin_span();
        let precision = self.accept_map_token(|token| token.to_precision_qualifier());
        let no_precision = self.parse_type_specifier_no_precision()?;
        Ok(span.end(
            self,
            TypeSpecifier {
                precision,
                no_precision,
            },
        ))
    }

    fn parse_type_specifier_no_precision(&mut self) -> Result<TypeSpecifierNoPrecision> {
        match self.peek_token() {
            Token::Struct => {
                self.expect_token(Token::Struct)?;
                let name = self.accept_map_token(|token| token.to_identifier());
                self.expect_token(Token::LeftBrace)?;
                let mut declarations = vec![];
                loop {
                    declarations.push(self.parse_struct_declaration()?);
                    if self.accept_token(Token::RightBrace) {
                        break;
                    }
                }
                Ok(TypeSpecifierNoPrecision::Struct { name, declarations })
            }
            Token::Identifier => {
                let mut names = vec![self.expect_map_token(|token| token.to_identifier())?];
                while self.accept_token(Token::PathSeparator) {
                    names.push(self.expect_map_token(|token| token.to_identifier())?);
                }
                Ok(TypeSpecifierNoPrecision::Path { names })
            },
            _ => {
                let name = self.expect_map_token(|token| token.to_type_identifier())?;
                Ok(TypeSpecifierNoPrecision::TypeIdentifier(name))
            }
        }
    }

    fn parse_struct_declaration(&mut self) -> Result<StructDeclaration> {
        let type_ = self.parse_type_specifier()?;
        let mut declarators = vec![];
        loop {
            declarators.push(self.parse_declarator()?);
            if !self.accept_token(Token::Comma) {
                break;
            }
        }
        self.expect_token(Token::Semicolon)?;
        Ok(StructDeclaration { type_, declarators })
    }

    fn parse_init_declarator(&mut self) -> Result<InitDeclarator> {
        self.parse_alternatives(&mut [
            &mut |this| {
                Ok(InitDeclarator::NameInitializer(
                    this.parse_name_initializer()?,
                ))
            },
            &mut |this| Ok(InitDeclarator::Declarator(this.parse_declarator()?)),
        ])
    }

    fn parse_declarator(&mut self) -> Result<DeclaratorWithSpan> {
        let span = self.begin_span();
        let name = self.expect_map_token(|token| token.to_identifier())?;
        let length = if self.accept_token(Token::LeftBracket) {
            let length = self.parse_conditional_expression()?;
            self.expect_token(Token::RightBracket)?;
            Some(length)
        } else {
            None
        };
        Ok(span.end(self, Declarator { name, length }))
    }

    fn parse_name_initializer(&mut self) -> Result<NameInitializerWithSpan> {
        let span = self.begin_span();
        let name = self.expect_map_token(|token| token.to_identifier())?;
        self.expect_token(Token::Eq)?;
        let initializer = self.parse_assignment_expression()?;
        Ok(span.end(self, NameInitializer { name, initializer }))
    }

    fn parse_statement(&mut self) -> Result<StatementWithSpan> {
        let span = self.begin_span();
        match self.peek_token() {
            Token::LeftBrace => {
                let compound_statement = self.parse_compound_statement()?;
                Ok(span.end(self, Statement::Compound(compound_statement)))
            }
            Token::If => {
                self.expect_token(Token::If)?;
                let condition;
                let consequent;
                let alternative;
                if self.accept_token(Token::LeftParen) {
                    condition = self.parse_expression()?;
                    self.expect_token(Token::RightParen)?;
                    consequent = Box::new(self.parse_statement()?);
                    alternative = if self.accept_token(Token::Else) {
                        Some(Box::new(self.parse_statement()?))
                    } else {
                        None
                    };
                } else {
                    condition = self.parse_expression()?;
                    consequent = Box::new(self.parse_compound_statement()?.into());
                    alternative = if self.accept_token(Token::Else) {
                        Some(Box::new(self.parse_compound_statement()?.into()))
                    } else {
                        None
                    };
                }
                Ok(span.end(
                    self,
                    Statement::If {
                        condition,
                        consequent,
                        alternative,
                    },
                ))
            }
            Token::While => {
                self.expect_token(Token::While)?;
                let condition;
                let body;
                if self.accept_token(Token::LeftParen) {
                    condition = self.parse_condition()?;
                    self.expect_token(Token::RightParen)?;
                    body = Box::new(self.parse_statement()?);
                } else {
                    condition = self.parse_condition()?;
                    body = Box::new(self.parse_compound_statement()?.into());
                }
                Ok(span.end(self, Statement::While { condition, body }))
            }
            Token::Do => {
                self.expect_token(Token::Do)?;
                let body = Box::new(self.parse_statement()?);
                self.expect_token(Token::While)?;
                self.expect_token(Token::LeftParen)?;
                let condition = self.parse_expression()?;
                self.expect_token(Token::RightParen)?;
                self.expect_token(Token::Semicolon)?;
                Ok(span.end(self, Statement::DoWhile { body, condition }))
            }
            Token::For => {
                self.expect_token(Token::For)?;
                if self.accept_token(Token::LeftParen) {
                    let initialization = self.parse_declaration_or_expression_statement()?;
                    let condition = if !self.accept_token(Token::Semicolon) {
                        let condition = self.parse_condition()?;
                        self.expect_token(Token::Semicolon)?;
                        Some(condition)
                    } else {
                        None
                    };
                    let afterthought = if !self.accept_token(Token::RightParen) {
                        let afterthought = self.parse_expression()?;
                        self.expect_token(Token::RightParen)?;
                        Some(afterthought)
                    } else {
                        None
                    };
                    let body = Box::new(self.parse_statement()?);
                    Ok(span.end(
                        self,
                        Statement::For {
                            initialization,
                            condition,
                            afterthought,
                            body,
                        },
                    ))
                } else {
                    let name = self.expect_map_token(|token| token.to_identifier())?;
                    let start = self.parse_expression()?;
                    self.expect_token(Token::DotDot)?;
                    let end = self.parse_expression()?;
                    let body = Box::new(self.parse_compound_statement()?);
                    Ok(span.end(
                        self,
                        Statement::ForIn {
                            name,
                            start,
                            end,
                            body,
                        }
                    ))
                }
            }
            Token::Continue => {
                self.expect_token(Token::Continue)?;
                self.expect_token(Token::Semicolon)?;
                Ok(span.end(self, Statement::Continue))
            }
            Token::Break => {
                self.expect_token(Token::Break)?;
                self.expect_token(Token::Semicolon)?;
                Ok(span.end(self, Statement::Break))
            }
            Token::Return => {
                self.expect_token(Token::Return)?;
                let value = if !self.accept_token(Token::Semicolon) {
                    let value = self.parse_expression()?;
                    self.expect_token(Token::Semicolon)?;
                    Some(value)
                } else {
                    None
                };
                Ok(span.end(self, Statement::Return { value }))
            }
            Token::Discard => {
                self.expect_token(Token::Discard)?;
                self.expect_token(Token::Semicolon)?;
                Ok(span.end(self, Statement::Discard))
            }
            _ => {
                let declaration_or_expression_statement = self.parse_declaration_or_expression_statement()?;
                Ok(span.end(
                    self,
                    Statement::DeclarationOrExpression(declaration_or_expression_statement),
                ))
            }
        }
    }

    fn parse_compound_statement(&mut self) -> Result<CompoundStatementWithSpan> {
        let span = self.begin_span();
        self.expect_token(Token::LeftBrace)?;
        let mut statements = vec![];
        while !self.accept_token(Token::RightBrace) {
            statements.push(self.parse_statement()?);
        }
        Ok(span.end(self, CompoundStatement { statements }))
    }

    fn parse_declaration_or_expression_statement(&mut self) -> Result<DeclarationOrExpressionStatement> {
        self.parse_alternatives(&mut [
            &mut |this| {
                Ok(DeclarationOrExpressionStatement::Declaration(
                    this.parse_declaration()?,
                ))
            },
            &mut |this| {
                Ok(DeclarationOrExpressionStatement::ExpressionStatement(this.parse_expression_statement()?))
            },
        ])
    }

    fn parse_expression_statement(&mut self) -> Result<ExpressionStatementWithSpan> {
        let span = self.begin_span();
        let expression = if !self.accept_token(Token::Semicolon) {
            let expression = self.parse_expression()?;
            self.expect_token(Token::Semicolon)?;
            Some(expression)
        } else {
            None
        };
        Ok(span.end(self, ExpressionStatement {
            expression
        }))
    }

    fn parse_condition(&mut self) -> Result<Condition> {
        self.parse_alternatives(&mut [
            &mut |this| Ok(Condition::Expression(this.parse_expression()?)),
            &mut |this| Ok(Condition::NameInitializer(this.parse_name_initializer()?)),
        ])
    }

    fn parse_expression(&mut self) -> Result<ExpressionWithSpan> {
        let span = self.begin_span();
        let expression = self.parse_assignment_expression()?;
        if self.accept_token(Token::Comma) {
            let mut expressions = vec![expression];
            while self.accept_token(Token::Comma) {
                expressions.push(self.parse_assignment_expression()?);
            }
            Ok(span.end(self, Expression::Sequence { expressions }))
        } else {
            Ok(expression)
        }
    }

    fn parse_assignment_expression(&mut self) -> Result<ExpressionWithSpan> {
        let span = self.begin_span();
        let expression = self.parse_conditional_expression()?;
        if let Some(operator) = self.accept_map_token(|token| token.to_assignment_operator()) {
            let left_operand = Box::new(expression);
            let right_operand = Box::new(self.parse_assignment_expression()?);
            Ok(span.end(
                self,
                Expression::Binary {
                    operator,
                    left_operand,
                    right_operand,
                },
            ))
        } else {
            Ok(expression)
        }
    }

    fn parse_conditional_expression(&mut self) -> Result<ExpressionWithSpan> {
        let span = self.begin_span();
        let mut accumulator = self.parse_logical_or_expression()?;
        while self.accept_token(Token::Question) {
            let condition = Box::new(accumulator);
            let consequent = Box::new(self.parse_expression()?);
            self.expect_token(Token::Colon)?;
            let alternative = Box::new(self.parse_conditional_expression()?);
            accumulator = span.clone().end(
                self,
                Expression::Conditional {
                    condition,
                    consequent,
                    alternative,
                },
            );
        }
        Ok(accumulator)
    }

    fn parse_logical_or_expression(&mut self) -> Result<ExpressionWithSpan> {
        let span = self.begin_span();
        let mut accumulator = self.parse_logical_xor_expression()?;
        while let Some(operator) = self.accept_map_token(|token| token.to_logical_or_operator()) {
            let left_operand = Box::new(accumulator);
            let right_operand = Box::new(self.parse_logical_xor_expression()?);
            accumulator = span.clone().end(
                self,
                Expression::Binary {
                    operator,
                    left_operand,
                    right_operand,
                },
            );
        }
        Ok(accumulator)
    }

    fn parse_logical_xor_expression(&mut self) -> Result<ExpressionWithSpan> {
        let span = self.begin_span();
        let mut accumulator = self.parse_logical_and_expression()?;
        while let Some(operator) = self.accept_map_token(|token| token.to_logical_xor_operator()) {
            let left_operand = Box::new(accumulator);
            let right_operand = Box::new(self.parse_logical_and_expression()?);
            accumulator = span.clone().end(
                self,
                Expression::Binary {
                    operator,
                    left_operand,
                    right_operand,
                },
            );
        }
        Ok(accumulator)
    }

    fn parse_logical_and_expression(&mut self) -> Result<ExpressionWithSpan> {
        let span = self.begin_span();
        let mut accumulator = self.parse_equality_expression()?;
        while let Some(operator) = self.accept_map_token(|token| token.to_logical_and_operator()) {
            let left_operand = Box::new(accumulator);
            let right_operand = Box::new(self.parse_equality_expression()?);
            accumulator = span.clone().end(
                self,
                Expression::Binary {
                    operator,
                    left_operand,
                    right_operand,
                },
            );
        }
        Ok(accumulator)
    }

    fn parse_equality_expression(&mut self) -> Result<ExpressionWithSpan> {
        let span = self.begin_span();
        let mut accumulator = self.parse_relational_expression()?;
        while let Some(operator) = self.accept_map_token(|token| token.to_equality_operator()) {
            let left_operand = Box::new(accumulator);
            let right_operand = Box::new(self.parse_relational_expression()?);
            accumulator = span.clone().end(
                self,
                Expression::Binary {
                    operator,
                    left_operand,
                    right_operand,
                },
            );
        }
        Ok(accumulator)
    }

    fn parse_relational_expression(&mut self) -> Result<ExpressionWithSpan> {
        let span = self.begin_span();
        let mut accumulator = self.parse_additive_expression()?;
        while let Some(operator) = self.accept_map_token(|token| token.to_relational_operator()) {
            let left_operand = Box::new(accumulator);
            let right_operand = Box::new(self.parse_additive_expression()?);
            accumulator = span.clone().end(
                self,
                Expression::Binary {
                    operator,
                    left_operand,
                    right_operand,
                },
            );
        }
        Ok(accumulator)
    }

    fn parse_additive_expression(&mut self) -> Result<ExpressionWithSpan> {
        let span = self.begin_span();
        let mut accumulator = self.parse_multiplicative_expression()?;
        while let Some(operator) = self.accept_map_token(|token| token.to_additive_operator()) {
            let left_operand = Box::new(accumulator);
            let right_operand = Box::new(self.parse_multiplicative_expression()?);
            accumulator = span.clone().end(
                self,
                Expression::Binary {
                    operator,
                    left_operand,
                    right_operand,
                },
            );
        }
        Ok(accumulator)
    }

    fn parse_multiplicative_expression(&mut self) -> Result<ExpressionWithSpan> {
        let span = self.begin_span();
        let mut accumulator = self.parse_unary_expression()?;
        while let Some(operator) = self.accept_map_token(|token| token.to_multiplicative_operator())
        {
            let left_operand = Box::new(accumulator);
            let right_operand = Box::new(self.parse_unary_expression()?);
            accumulator = span.clone().end(
                self,
                Expression::Binary {
                    operator,
                    left_operand,
                    right_operand,
                },
            );
        }
        Ok(accumulator)
    }

    fn parse_unary_expression(&mut self) -> Result<ExpressionWithSpan> {
        let span = self.begin_span();
        if let Some(operator) = self.accept_map_token(|token| token.to_unary_operator()) {
            let operand = Box::new(self.parse_unary_expression()?);
            Ok(span.end(self, Expression::Unary { operator, operand }))
        } else {
            self.parse_postfix_expression()
        }
    }

    fn parse_postfix_expression(&mut self) -> Result<ExpressionWithSpan> {
        let span = self.begin_span();
        let mut accumulator = self.parse_alternatives(&mut [
            &mut |this| {
                let span = this.begin_span();
                let name = this.expect_map_token(|token| token.to_function_identifier())?;
                this.expect_token(Token::LeftParen)?;
                let mut arguments = vec![];
                if !this.accept_token(Token::RightParen) {
                    loop {
                        arguments.push(this.parse_assignment_expression()?);
                        if !this.accept_token(Token::Comma) {
                            break;
                        }
                    }
                    this.expect_token(Token::RightParen)?;
                }
                Ok(span.end(this, Expression::Call { name, arguments }))
            },
            &mut |this| this.parse_primary_expression(),
        ])?;
        loop {
            match self.peek_token() {
                Token::Dot => {
                    let expression = Box::new(accumulator);
                    self.expect_token(Token::Dot)?;
                    let name = self.expect_map_token(|token| token.to_identifier())?;
                    accumulator = span
                        .clone()
                        .end(self, Expression::Field { expression, name });
                }
                Token::LeftBracket => {
                    let expression = Box::new(accumulator);
                    self.expect_token(Token::LeftBracket)?;
                    let index = Box::new(self.parse_expression()?);
                    self.expect_token(Token::RightBracket)?;
                    accumulator = span
                        .clone()
                        .end(self, Expression::Element { expression, index });
                }
                _ => {
                    if let Some(operator) =
                        self.accept_map_token(|token| token.to_postfix_operator())
                    {
                        let operand = Box::new(accumulator);
                        accumulator = span
                            .clone()
                            .end(self, Expression::Unary { operator, operand });
                    } else {
                        break;
                    }
                }
            }
        }
        Ok(accumulator)
    }

    fn parse_primary_expression(&mut self) -> Result<ExpressionWithSpan> {
        let span = self.begin_span();
        match self.peek_token() {
            Token::BoolConstant(value) => {
                self.skip_token();
                Ok(span.end(self, Expression::BoolConstant(value)))
            }
            Token::IntConstant(value) => {
                self.skip_token();
                Ok(span.end(self, Expression::IntConstant(value)))
            }
            Token::FloatConstant(value) => {
                self.skip_token();
                Ok(span.end(self, Expression::FloatConstant(value)))
            }
            Token::Identifier => {
                self.skip_token();
                Ok(span.end(self, Expression::Identifier))
            }
            Token::LeftParen => {
                self.skip_token();
                let expression = self.parse_expression()?;
                self.expect_token(Token::RightParen)?;
                Ok(expression)
            }
            token => Err(self.error(ErrorKind::UnexpectedToken(token))),
        }
    }

    fn parse_alternatives<T>(
        &mut self,
        fs: &mut [&mut dyn FnMut(&mut Parser) -> Result<T>],
    ) -> Result<T> {
        let start = self.index;
        let mut current_err = match fs[0](self) {
            Ok(ok) => return Ok(ok),
            Err(err) => err,
        };
        let mut current_length = self.index - start;
        for f in &mut fs[1..] {
            self.index = start;
            let err = match f(self) {
                Ok(ok) => return Ok(ok),
                Err(err) => err,
            };
            let length = self.index - start;
            if length < current_length {
                continue;
            }
            current_err = err;
            current_length = length;
        }
        Err(current_err)
    }

    fn expect_token(&mut self, expected: Token) -> Result<()> {
        let actual = self.peek_token();
        if actual == expected {
            self.skip_token();
            Ok(())
        } else {
            Err(self.error(ErrorKind::UnexpectedToken(actual)))
        }
    }

    fn expect_map_token<T, F>(&mut self, mut f: F) -> Result<T>
    where
        T: FromInnerAndSpan,
        F: FnMut(Token) -> Option<T::Inner>,
    {
        let span = self.begin_span();
        let actual = self.peek_token();
        if let Some(mapped) = f(actual) {
            self.skip_token();
            Ok(span.end(self, mapped))
        } else {
            Err(self.error(ErrorKind::UnexpectedToken(actual)))
        }
    }

    fn accept_token(&mut self, expected: Token) -> bool {
        self.expect_token(expected).ok().is_some()
    }

    fn accept_map_token<T, F>(&mut self, f: F) -> Option<T>
    where
        T: FromInnerAndSpan,
        F: FnMut(Token) -> Option<T::Inner>,
    {
        self.expect_map_token(f).ok()
    }

    fn begin_span(&mut self) -> SpanTracker {
        SpanTracker {
            start: self.tokens[self.index].span.start,
        }
    }

    fn error(&self, kind: ErrorKind) -> Error {
        Error {
            span: self.tokens[self.index].span,
            kind,
        }
    }

    fn peek_token(&mut self) -> Token {
        self.tokens[self.index].token
    }

    fn skip_token(&mut self) {
        self.index += 1
    }
}

pub type Result<T> = result::Result<T, Error>;

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub struct Error {
    pub span: Span,
    pub kind: ErrorKind,
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub enum ErrorKind {
    UnexpectedToken(Token),
}

#[derive(Clone, Debug)]
struct SpanTracker {
    start: usize,
}

impl SpanTracker {
    fn end<T>(self, parser: &mut Parser, inner: T::Inner) -> T
    where
        T: FromInnerAndSpan,
    {
        T::from_inner_and_span(
            inner,
            Span::new(self.start, parser.tokens[parser.index - 1].span.end),
        )
    }
}
