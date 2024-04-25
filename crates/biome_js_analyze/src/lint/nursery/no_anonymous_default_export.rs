use biome_analyze::{context::RuleContext, declare_rule, Ast, Rule, RuleDiagnostic, RuleSource};
use biome_console::markup;
use biome_js_syntax::{
    AnyJsBinding, AnyJsExportClause, AnyJsExportDefaultDeclaration, AnyJsExpression, JsExport,
    JsIdentifierExpression, JsReferenceIdentifier, TsIdentifierBinding,
};
use biome_rowan::{declare_node_union, AstNode};

declare_rule! {
    /// Disallow anonymous default exports.
    ///
    /// Anonymous default exports cannot be easily discovered inside an editor:
    /// They cannot be suggested by the editor when the user tries to import a name.
    ///
    /// The same identifier will be reused in the declarations and on the import side, thus improving searchability.
    ///
    /// Note that this rule disallows only default exports in EcmaScript Module.
    /// It ignores CommonJS default exports.
    ///
    /// ## Examples
    ///
    /// ### Invalid
    ///
    /// ```js,expect_diagnostic
    /// export default []
    /// ```
    ///
    /// ```js,expect_diagnostic
    /// export default () => {}
    /// ```
    ///
    /// ```js,expect_diagnostic
    /// export default class {}
    /// ```
    ///
    /// ```js,expect_diagnostic
    /// export default function () {}
    /// ```
    ///
    /// ```js,expect_diagnostic
    /// export default new Foo()
    /// ```
    ///
    /// ### Valid
    ///
    /// ```js
    /// const foo = []
    /// export default foo
    /// ```
    ///
    /// ```js
    /// export default function foo() {}
    /// ```
    ///
    /// ```js
    /// export default class MyClass {}
    /// ```
    ///
    pub NoAnonymousDefaultExport {
        version: "next",
        name: "noAnonymousDefaultExport",
        sources: &[RuleSource::EslintImport("no-anonymous-default-export")],
        recommended: false,
    }
}

declare_node_union! {
    pub AnyDefaultExport = AnyJsBinding | TsIdentifierBinding | JsReferenceIdentifier
}

pub enum DefaultExported {
    Class(Option<AnyJsBinding>),
    Function(Option<AnyJsBinding>),
    Interface(Option<TsIdentifierBinding>),
    Identifier(JsIdentifierExpression),
    LiteralExpression,
    ObjectExpression,
    ArrayExpression,
    CallExpression,
    ArrowFunctionExpression,
    NewExpression,
}

impl DefaultExported {
    fn has_id(&self) -> bool {
        match self {
            DefaultExported::Class(id) => id.is_some(),
            DefaultExported::Function(id) => id.is_some(),
            DefaultExported::Interface(id) => id.is_some(),
            DefaultExported::Identifier(_) => true,
            _ => false,
        }
    }
    fn anonymous_object_name(&self) -> &str {
        match self {
            DefaultExported::Class(_) => "anonymous class",
            DefaultExported::Function(_) => "anonymous function",
            DefaultExported::Interface(_) => "anonymous interface",
            DefaultExported::Identifier(_) => "anonymous identifier",
            DefaultExported::LiteralExpression => "literal expression",
            DefaultExported::ObjectExpression => "object expression",
            DefaultExported::ArrayExpression => "array expression",
            DefaultExported::CallExpression => "call expression",
            DefaultExported::ArrowFunctionExpression => "arrow function expression",
            DefaultExported::NewExpression => "new expression",
        }
    }
    fn suggest_other_way(&self) -> &str {
        match self {
            DefaultExported::Class(_) => "use a named class",
            DefaultExported::Function(_) => "use a named function",
            DefaultExported::Interface(_) => "use a named interface",
            DefaultExported::Identifier(_) => "use a named identifier",
            DefaultExported::LiteralExpression => "assign to a variable",
            DefaultExported::ObjectExpression => "assign to a variable",
            DefaultExported::ArrayExpression => "assign to a variable",
            DefaultExported::CallExpression => "assign to a variable",
            DefaultExported::ArrowFunctionExpression => "assign to a variable",
            DefaultExported::NewExpression => "assign to a variable",
        }
    }
}

impl Rule for NoAnonymousDefaultExport {
    type Query = Ast<JsExport>;
    type State = DefaultExported;
    type Signals = Option<Self::State>;
    type Options = ();

    fn run(ctx: &RuleContext<Self>) -> Self::Signals {
        let export = ctx.query();
        export.export_clause().ok().and_then(|clause| {
            let exported = match clause {
                AnyJsExportClause::JsExportDefaultDeclarationClause(default_clause) => {
                    let export_default_declaration = default_clause.declaration().ok()?;
                    match export_default_declaration {
                        AnyJsExportDefaultDeclaration::JsClassExportDefaultDeclaration(class) => DefaultExported::Class(class.id()),
                        AnyJsExportDefaultDeclaration::JsFunctionExportDefaultDeclaration(function) => DefaultExported::Function(function.id()),
                        AnyJsExportDefaultDeclaration::TsDeclareFunctionExportDefaultDeclaration(function_declaration) => DefaultExported::Function(function_declaration.id()),
                        AnyJsExportDefaultDeclaration::TsInterfaceDeclaration(interface) => DefaultExported::Interface(interface.id().ok()),
                    }
                }
                AnyJsExportClause::JsExportDefaultExpressionClause(expression) => {
                    let export_expression = expression.expression().ok()?;
                    match export_expression {
                        AnyJsExpression::AnyJsLiteralExpression(_) => DefaultExported::LiteralExpression,
                        AnyJsExpression::JsArrayExpression(_) => DefaultExported::ArrayExpression,
                        AnyJsExpression::JsCallExpression(_) => DefaultExported::CallExpression,
                        AnyJsExpression::JsArrowFunctionExpression(_) => DefaultExported::ArrowFunctionExpression,
                        AnyJsExpression::JsNewExpression(_) => DefaultExported::NewExpression,
                        AnyJsExpression::JsIdentifierExpression(id) => DefaultExported::Identifier(id),
                        AnyJsExpression::JsObjectExpression(_) => DefaultExported::ObjectExpression,
                        _ => return None,
                    }
                }
                _ => {
                    return None
                },
            };
            if exported.has_id(){
                None
            }else{
                Some(exported)
            }
        })
    }

    fn diagnostic(ctx: &RuleContext<Self>, state: &Self::State) -> Option<RuleDiagnostic> {
        let node = ctx.query();
        Some(
            RuleDiagnostic::new(
                rule_category!(),
                node.range(),
                markup! {
                    "Avoid "<Emphasis>"anonymous default"</Emphasis>" exports. (" {state.anonymous_object_name()} ")"
                },
            )
            .note(markup! {
                "Anonymous exports inhibit naming consistency in code."
            })
            .note(markup! {
                "It is necessary to " {state.suggest_other_way()} " instead."
            })
        )
    }
}
