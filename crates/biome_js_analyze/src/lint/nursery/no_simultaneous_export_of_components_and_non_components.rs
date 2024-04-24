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
    JsIdentifierExpression, JsLanguage, JsLiteralExportName, JsReferenceIdentifier, TsIdentifierBinding, AnyTsType, TsEnumDeclaration
};
use biome_rowan::declare_node_union;
use biome_rowan::{AstNode, Language, TextRange, WalkEvent};
use biome_string_case::Case;
use serde::{Deserialize, Serialize};
use Iterator;

declare_rule! {
    /// React components and regular functions must be exported in separate files.
    /// This is necessary to enable the Fast Refresh feature, which streamlines development.
    /// If they are being exported together, the files need to be split.
    ///
    /// https://www.npmjs.com/package/eslint-plugin-react-refresh
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
    pub NoSimultaneousExportOfComponentsAndNonComponents {
        version: "next",
        name: "noSimultaneousExportOfComponentsAndNonComponents",
        recommended: false,
    }
}

declare_node_union! {
    pub AnyIdentifier = JsReferenceIdentifier | AnyJsBindingPattern | AnyJsBinding | JsIdentifierExpression | JsLiteralExportName | TsIdentifierBinding
}

#[derive(Default)]
struct AnyNonComponentsExportInJsxVisitor {
    has_components_exports: bool,
    non_compontents_exports: Vec<ExportedItem>,
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
                    let x = get_exported_items(&export);
                    for exported_item in x {
                        if let Some(AnyJsExported::AnyTsType(_)) = exported_item.exported {
                            continue;
                        }
                        let acturaucase = Case::identify(&exported_item.identifier.text(), false);
                        let is_component = acturaucase == Case::Pascal && match exported_item.exported.clone() {
                            Some(exported) => is_maybe_react_component(exported),
                            None => true,
                        };
                        if is_component {
                            self.has_components_exports = true;
                        } else {
                            self.non_compontents_exports.push(exported_item);
                        }
                    }
                }
            }
            WalkEvent::Leave(node) => {
                if AnyJsRoot::cast_ref(node).is_some() && self.has_components_exports {
                    for exported_item in self.non_compontents_exports.iter() {
                        ctx.match_query(AnyNonComponentsExportInJsx(exported_item.clone()));
                    }
                }
            }
        }
    }
}

pub struct AnyNonComponentsExportInJsx(ExportedItem);

impl QueryMatch for AnyNonComponentsExportInJsx {
    fn text_range(&self) -> TextRange {
        self.0.identifier.range()
    }
}

impl Queryable for AnyNonComponentsExportInJsx {
    type Input = Self;
    type Language = JsLanguage;
    type Output = ExportedItem;
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
        let exported_item = ctx.query();
        let is_export_name_queryable = !ctx
            .options()
            .ignore_export_names
            .contains(&exported_item.identifier.text());
        let is_export_value_queryable = !(ctx.options().ignore_constant_export
            && exported_item
                .exported
                .clone()
                .map(|partof| match partof {
                    AnyJsExported::AnyJsExpression(expr) => is_literal(expr),
                    _ => false,
                })
                .unwrap_or(false));
        if is_export_name_queryable && is_export_value_queryable {
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
                node.identifier.range(),
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

declare_node_union! {
    pub AnyJsExported = AnyJsExpression | AnyJsExportClause | AnyTsType | TsEnumDeclaration
}

#[derive(Clone)]
pub struct ExportedItem {
    identifier: AnyIdentifier,
    exported: Option<AnyJsExported>,
}

fn get_exported_items(export: &JsExport) -> Vec<ExportedItem> {
    export
        .export_clause()
        .ok()
        .and_then(|export_clause| match export_clause {
            // export const x = 100;
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
                                            declarator.id().ok().map(|id| ExportedItem {
                                                identifier: AnyIdentifier::AnyJsBindingPattern(id),
                                                exported: declarator.initializer().and_then(
                                                    |initializer_clause| {
                                                        initializer_clause.expression().ok()
                                                    },
                                                ).map(AnyJsExported::AnyJsExpression),
                                            })
                                        })
                                    })
                                    .collect()
                            })
                    }
                    AnyJsDeclarationClause::JsFunctionDeclaration(function_declaration_clause) => {
                        function_declaration_clause.id().ok().map(|function_id| {
                            vec![
                                ExportedItem {
                                    identifier: AnyIdentifier::AnyJsBinding(function_id),
                                    exported: None,
                                }
                            ]
                        })
                    }
                    AnyJsDeclarationClause::TsEnumDeclaration(ts_enum_declaration) => {
                        ts_enum_declaration.id().ok().map(|enum_id| vec![ExportedItem {
                            identifier: AnyIdentifier::AnyJsBinding(enum_id),
                            exported: Some(AnyJsExported::TsEnumDeclaration(ts_enum_declaration)),
                        }])
                    },
                    AnyJsDeclarationClause::TsTypeAliasDeclaration(ts_type_alias_declaration) => {
                        ts_type_alias_declaration.binding_identifier().ok().map(|type_alias_id| vec![ExportedItem {
                            identifier: AnyIdentifier::TsIdentifierBinding(type_alias_id),
                            exported: ts_type_alias_declaration.ty().ok().map(AnyJsExported::AnyTsType),
                        }])
                    },
                    _ => None
                }
            }
            AnyJsExportClause::JsExportDefaultDeclarationClause(default_declaration_clause) => {
                default_declaration_clause
                    .declaration()
                    .ok()
                    .and_then(|default_declation| match default_declation {
                        // export default function x() {}
                        AnyJsExportDefaultDeclaration::JsFunctionExportDefaultDeclaration(
                            function_declaration,
                        ) => function_declaration.id(),
                        // export default class x {}
                        AnyJsExportDefaultDeclaration::JsClassExportDefaultDeclaration(
                            class_declaration,
                        ) => class_declaration.id(),
                        _ => None,
                    })
                    .map(|any_js_binding| {
                        vec![
                            ExportedItem {
                                identifier: AnyIdentifier::AnyJsBinding(any_js_binding),
                                exported: None,
                            }
                        ]
                    })
            }
            // export default x;
            AnyJsExportClause::JsExportDefaultExpressionClause(clause) => clause
                .expression()
                .ok()
                .and_then(|expression| match expression {
                    AnyJsExpression::JsIdentifierExpression(identifier) => Some(vec![
                        ExportedItem {
                            identifier: AnyIdentifier::JsIdentifierExpression(identifier),
                            exported: None,
                        }
                    ]),
                    _ => None,
                }),
            // export { x, y, z };
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
                                specifier
                                    .exported_name()
                                    .ok()
                                    .map(AnyIdentifier::JsLiteralExportName)
                            }
                        }
                        .map(|any_identifier| ExportedItem {
                            identifier: any_identifier,
                            exported: None,
                        })
                    })
                    .collect(),
            ),
            _ => None,
        })
        .unwrap_or_default()
}

fn is_literal(expr: AnyJsExpression) -> bool {
    match expr {
        AnyJsExpression::AnyJsLiteralExpression(_) => true,
        AnyJsExpression::JsBinaryExpression(_) => true,
        AnyJsExpression::JsTemplateExpression(template_expression) => template_expression.tag().is_none(),
        _ => false,
    }
}

fn is_maybe_react_component(any_exported: AnyJsExported) -> bool {
    match any_exported {
        AnyJsExported::TsEnumDeclaration(_) => false,
        _ => true,
    }
}
