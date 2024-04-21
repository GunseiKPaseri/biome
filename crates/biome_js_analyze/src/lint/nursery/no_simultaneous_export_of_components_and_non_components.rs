//use std::any::Any;

use Iterator;
use biome_analyze::{
    context::RuleContext, declare_rule, AddVisitor, Phases, QueryMatch, Queryable, Rule,
    RuleDiagnostic, ServiceBag, Visitor,
};
use biome_console::markup;
use biome_string_case::Case;
use biome_js_syntax::{
    AnyJsBinding, AnyJsBindingPattern, AnyJsDeclarationClause, AnyJsExportDefaultDeclaration, AnyJsRoot, JsExport, JsLanguage, JsReferenceIdentifier, AnyJsExpression, JsIdentifierExpression
    // JsLiteralExportName, JsVariableDeclarator,
};
use biome_rowan::declare_node_union;
use biome_rowan::{AstNode, Language, TextRange, WalkEvent};

// use biome_analyze::{
//     ActionCategory, Ast, FixKind, 
//     RuleSource,
// };
// use biome_diagnostics::Applicability;
// use biome_js_syntax::{AnyJsModuleItem,  JsModuleItemList, JsSyntaxToken};
// use biome_rowan::{ AstSeparatedList, BatchMutationExt};

// use biome_js_syntax::{
//     AnyJsClassMemberName, JsClassMemberList, JsGetterClassMember, JsMethodClassMember,
//     JsPropertyClassMember, JsSetterClassMember, JsStaticModifier, JsSyntaxList,
// };
// use biome_rowan::{AstNodeList, TokenText};
// use rustc_hash::{FxHashMap, FxHashSet};

declare_rule! {
    /// Succinct description of the rule.
    ///
    /// Put context and details about the rule.
    /// As a starting point, you can take the description of the corresponding _ESLint_ rule (if any).
    ///
    /// Try to stay consistent with the descriptions of implemented rules.
    ///
    /// Add a link to the corresponding ESLint rule (if any):
    ///
    /// ## Examples
    ///
    /// ### Invalid
    ///
    /// ```js,expect_diagnostic
    /// var a = 1;
    /// a = 2;
    /// ```
    ///
    /// ### Valid
    ///
    /// ```js
    /// // var a = 1;
    /// ```
    ///
    pub NoSimultaneousExportOfComponentsAndNonComponents {
        version: "next",
        name: "noSimultaneousExportOfComponentsAndNonComponents",
        recommended: false,
    }
}


declare_node_union! {
    pub AnyIdentifier = JsReferenceIdentifier | AnyJsBindingPattern | AnyJsBinding | JsIdentifierExpression
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
                    if let Some(export_clause) = export.export_clause().ok() {
                        if let Some(default_declaration_clause) = export_clause.as_js_export_default_declaration_clause() {
                            if let Some(default_declation) = default_declaration_clause.declaration().ok() {
                                match default_declation {
                                    AnyJsExportDefaultDeclaration::JsFunctionExportDefaultDeclaration(function_declaration) => {
                                        if let Some(function_id) = function_declaration.id() {
                                            let acturaucase = Case::identify(&function_id.text(), false);
                                            // same
                                            if acturaucase == Case::Pascal {
                                                self.has_components_exports = true;
                                            } else {
                                                self.non_compontents_exports.push(AnyIdentifier::AnyJsBinding(function_id));
                                            }
                                            // /same
                                        }
                                    },
                                    AnyJsExportDefaultDeclaration::JsClassExportDefaultDeclaration(class_declaration) => {
                                        if let Some(class_id) = class_declaration.id() {
                                            let acturaucase = Case::identify(&class_id.text(), false);
                                            // same
                                            if acturaucase == Case::Pascal {
                                                self.has_components_exports = true;
                                            } else {
                                                self.non_compontents_exports.push(AnyIdentifier::AnyJsBinding(class_id));
                                            }
                                            // /same
                                        }
                                    },
                                    _ => {}
                                }
                            }
                        }
                        if let Some(clause) = export_clause.as_js_export_default_expression_clause() {
                            if let Some(expression) = clause.expression().ok() {
                                match expression {
                                    AnyJsExpression::JsIdentifierExpression(identifier) => {
                                        let acturaucase = Case::identify(&identifier.text(), false);
                                        // same
                                        if acturaucase == Case::Pascal {
                                            self.has_components_exports = true;
                                        } else {
                                            self.non_compontents_exports.push(AnyIdentifier::JsIdentifierExpression(identifier));
                                        }
                                        // /same
                                    }
                                    _ => {}
                                }
                            }
                        }
                        if let Some(clause) = export_clause.as_js_export_named_from_clause() {
                            println!("FUGAGA4!!!!!!{:?}", clause)
                        }
                        if let Some(named_clause) = export_clause.as_js_export_named_clause() {
                            for export_specifier in named_clause.specifiers().into_iter().filter_map(|r| r.ok()) {
                                if let Some(identifier) = export_specifier.local_name().ok() {
                                    // same
                                    let acturaucase = Case::identify(&identifier.text(), false);
                                    if acturaucase == Case::Pascal {
                                        self.has_components_exports = true;
                                    } else {
                                        self.non_compontents_exports.push(AnyIdentifier::JsReferenceIdentifier(identifier));
                                    }
                                    // /same
                                }
                            }
                        }
                        if let Some(declaration_clause) = export_clause.as_any_js_declaration_clause() {
                            match declaration_clause {
                                AnyJsDeclarationClause::JsVariableDeclarationClause(variable_declaration_clause) => {
                                    if let Some(variable_declaration) =  variable_declaration_clause.declaration().ok() {
                                        for variable_declarator in variable_declaration.declarators().into_iter().filter_map(|r| r.ok()) {
                                            if let Some(identifier) = variable_declarator.id().ok() {
                                                let acturaucase = Case::identify(&identifier.text(), false);
                                                // same
                                                if acturaucase == Case::Pascal {
                                                    self.has_components_exports = true;
                                                } else {
                                                    self.non_compontents_exports.push(AnyIdentifier::AnyJsBindingPattern(identifier));
                                                }
                                                // /same
                                            }
                                        }
                                    }
                                },
                                AnyJsDeclarationClause::JsFunctionDeclaration(function_declaration_clause) => {
                                    if let Some(function_id) = function_declaration_clause.id().ok() {
                                        let acturaucase = Case::identify(&function_id.text(), false);
                                        // same
                                        if acturaucase == Case::Pascal {
                                            self.has_components_exports = true;
                                        } else {
                                            self.non_compontents_exports.push(AnyIdentifier::AnyJsBinding(function_id));
                                        }
                                        // /same
                                    }
                                },
                                _ => {}
                            }
                        }
                        // if let Some(named_clause) = export_clause.as_js_export_named_clause() {
                        // }
                    }else{
                        println!("OTHEREXPORT!!!!{:?}", export);
                    }
                }
            }
            WalkEvent::Leave(node) => {
                if AnyJsRoot::cast_ref(node).is_some() && self.has_components_exports {
                    println!("HOGEGEGE! {:?}", self.non_compontents_exports);
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


impl Rule for NoSimultaneousExportOfComponentsAndNonComponents {
    type Query = AnyNonComponentsExportInJsx;
    type State = ();
    type Signals = Option<Self::State>;
    type Options = ();

    fn run(_ctx: &RuleContext<Self>) -> Self::Signals {
        Some(())
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
                    "In JSX, export only components."
                },
            )
            .note(markup! {
                "This note will give you more information."
            }),
        )
    }
}
