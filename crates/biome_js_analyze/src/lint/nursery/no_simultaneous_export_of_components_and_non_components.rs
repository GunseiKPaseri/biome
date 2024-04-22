//use std::any::Any;

use biome_analyze::{
    context::RuleContext, declare_rule, AddVisitor, Phases, QueryMatch, Queryable, Rule,
    RuleDiagnostic, ServiceBag, Visitor,
};
use biome_console::markup;
use biome_deserialize_macros::Deserializable;
use biome_js_syntax::{
    AnyJsBinding, AnyJsBindingPattern, AnyJsDeclarationClause, AnyJsExportClause,
    AnyJsExportDefaultDeclaration, AnyJsExportNamedSpecifier, AnyJsExpression, AnyJsRoot, JsExport,
    JsIdentifierExpression, JsLanguage, JsLiteralExportName, JsReferenceIdentifier,
};
use biome_rowan::declare_node_union;
use biome_rowan::{AstNode, Language, TextRange, WalkEvent};
use biome_string_case::Case;
use serde::{Deserialize, Serialize};
use Iterator;

declare_rule! {
    /// React components and regular functions must be exported in separate files.
    /// If they are being exported together, the files need to be split.
    ///
    /// [TODO]Add a link to the corresponding ESLint rule (if any):
    ///
    /// ## Examples
    ///
    /// ### Invalid
    ///
    /// ```js,expect_diagnostic
    /// export const SampleComponent = () => <></>
    /// export const test = 100
    /// export function hoge () {
    ///   return 100
    /// }
    /// ```
    ///
    /// ### Valid
    ///
    /// ```js
    /// export const SampleComponent = () => <></>
    /// ```
    ///
    /// ```js
    /// export const test = 100
    /// export function hoge() {
    ///   return 100
    /// }
    /// ```
    ///
    pub NoSimultaneousExportOfComponentsAndNonComponents {
        version: "next",
        name: "noSimultaneousExportOfComponentsAndNonComponents",
        recommended: false,
    }
}

declare_node_union! {
    pub AnyIdentifier = JsReferenceIdentifier | AnyJsBindingPattern | AnyJsBinding | JsIdentifierExpression | JsLiteralExportName
}

#[derive(Default)]
struct AnyNonComponentsExportInJsxVisitor {
    has_components_exports: bool,
    non_compontents_exports: Vec<AnyIdentifier>,
}

impl Visitor for AnyNonComponentsExportInJsxVisitor {
    type Language = JsLanguage;

    fn visit(
        &mut self,
        event: &biome_rowan::WalkEvent<biome_rowan::SyntaxNode<Self::Language>>,
        mut ctx: biome_analyze::VisitorContext<Self::Language>,
    ) {
        match event {
            WalkEvent::Enter(node) => {
                if let Some(export) = JsExport::cast_ref(node) {
                    let x = get_exported_identifiers(&export);
                    for export_identifier in x {
                        let acturaucase = Case::identify(&export_identifier.text(), false);
                        if acturaucase == Case::Pascal {
                            self.has_components_exports = true;
                        } else {
                            self.non_compontents_exports.push(export_identifier);
                        }
                    }
                }
            }
            WalkEvent::Leave(node) => {
                if AnyJsRoot::cast_ref(node).is_some() && self.has_components_exports {
                    for export in self.non_compontents_exports.iter() {
                        ctx.match_query(AnyNonComponentsExportInJsx(export.clone()));
                    }
                }
            }
        }
    }
}

pub struct AnyNonComponentsExportInJsx(AnyIdentifier);

impl QueryMatch for AnyNonComponentsExportInJsx {
    fn text_range(&self) -> TextRange {
        self.0.range()
    }
}

impl Queryable for AnyNonComponentsExportInJsx {
    type Input = Self;
    type Language = JsLanguage;
    type Output = AnyIdentifier;
    type Services = ();

    fn build_visitor(
        analyzer: &mut impl AddVisitor<Self::Language>,
        _: &<Self::Language as Language>::Root,
    ) {
        analyzer.add_visitor(Phases::Syntax, AnyNonComponentsExportInJsxVisitor::default);
    }

    fn unwrap_match(_: &ServiceBag, query: &Self::Input) -> Self::Output {
        query.0.clone()
    }
}

fn is_default<T: Default + Eq>(value: &T) -> bool {
    value == &T::default()
}

#[derive(Debug, Clone, Deserialize, Deserializable, Eq, PartialEq, Serialize, Default)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct NoSimultaneousExportOfComponentsAndNonComponentsOptions {
    #[serde(default, skip_serializing_if = "is_default")]
    ignore_export_names: Vec<String>,
    #[serde(default, skip_serializing_if = "is_default")]
    ignore_constant_export: bool,
}

impl Rule for NoSimultaneousExportOfComponentsAndNonComponents {
    type Query = AnyNonComponentsExportInJsx;
    type State = ();
    type Signals = Option<Self::State>;
    type Options = NoSimultaneousExportOfComponentsAndNonComponentsOptions;

    fn run(ctx: &RuleContext<Self>) -> Self::Signals {
        let export_identifier = ctx.query();
        if !ctx
            .options()
            .ignore_export_names
            .contains(&export_identifier.text())
        {
            Some(())
        } else {
            None
        }
    }

    fn diagnostic(ctx: &RuleContext<Self>, _state: &Self::State) -> Option<RuleDiagnostic> {
        //
        // Read our guidelines to write great diagnostics:
        // https://docs.rs/biome_analyze/latest/biome_analyze/#what-a-rule-should-say-to-the-user
        //
        let node = ctx.query();
        Some(
            RuleDiagnostic::new(
                rule_category!(),
                node.range(),
                markup! {
                    "Components and non-components functions are exported at the same time."
                },
            )
            .note(markup! {
                "React components and regular functions must be exported in separate files."
            }),
        )
    }
}

fn get_exported_identifiers(export: &JsExport) -> Vec<AnyIdentifier> {
    export
        .export_clause()
        .ok()
        .and_then(|export_clause| match export_clause {
            AnyJsExportClause::AnyJsDeclarationClause(declaration_clause) => {
                match declaration_clause {
                    AnyJsDeclarationClause::JsVariableDeclarationClause(
                        variable_declaration_clause,
                    ) => {
                        variable_declaration_clause
                            .declaration()
                            .ok()
                            .map(|variable_declaration| {
                                variable_declaration
                                    .declarators()
                                    .into_iter()
                                    .filter_map(|declarator| {
                                        declarator.ok().and_then(|declarator| {
                                            declarator
                                                .id()
                                                .ok()
                                                .map(AnyIdentifier::AnyJsBindingPattern)
                                        })
                                    })
                                    .collect()
                            })
                    }
                    AnyJsDeclarationClause::JsFunctionDeclaration(function_declaration_clause) => {
                        function_declaration_clause
                            .id()
                            .ok()
                            .map(|function_id| vec![AnyIdentifier::AnyJsBinding(function_id); 1])
                    }
                    _ => None,
                }
            }
            AnyJsExportClause::JsExportDefaultDeclarationClause(default_declaration_clause) => {
                default_declaration_clause
                    .declaration()
                    .ok()
                    .and_then(|default_declation| match default_declation {
                        AnyJsExportDefaultDeclaration::JsFunctionExportDefaultDeclaration(
                            function_declaration,
                        ) => function_declaration
                            .id()
                            .map(|function_id| vec![AnyIdentifier::AnyJsBinding(function_id); 1]),
                        AnyJsExportDefaultDeclaration::JsClassExportDefaultDeclaration(
                            class_declaration,
                        ) => class_declaration
                            .id()
                            .map(|class_id| vec![AnyIdentifier::AnyJsBinding(class_id); 1]),
                        _ => None,
                    })
            }
            AnyJsExportClause::JsExportDefaultExpressionClause(clause) => clause
                .expression()
                .ok()
                .and_then(|expression| match expression {
                    AnyJsExpression::JsIdentifierExpression(identifier) => {
                        Some(vec![AnyIdentifier::JsIdentifierExpression(identifier); 1])
                    }
                    _ => None,
                }),
            AnyJsExportClause::JsExportNamedClause(named_clause) => Some(
                named_clause
                    .specifiers()
                    .into_iter()
                    .filter_map(|r| r.ok())
                    .filter_map(|export_specifier| {
                        match export_specifier {
                            AnyJsExportNamedSpecifier::JsExportNamedShorthandSpecifier(
                                shorthand,
                            ) => shorthand
                                .name()
                                .ok()
                                .map(AnyIdentifier::JsReferenceIdentifier),
                            AnyJsExportNamedSpecifier::JsExportNamedSpecifier(specifier) => {
                                specifier.exported_name().ok().map(AnyIdentifier::JsLiteralExportName)
                            }
                        }
                        // export_specifier
                        //     .local_name()
                        //     .ok()
                        //     .map(|identifier| AnyIdentifier::JsReferenceIdentifier(identifier))
                    })
                    .collect(),
            ),
            _ => None,
        })
        .unwrap_or_default()
}
