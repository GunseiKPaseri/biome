use biome_rowan::{declare_node_union, AstNode, SyntaxResult};

use crate::{
    AnyJsBinding, AnyJsBindingPattern, AnyJsDeclarationClause, AnyJsExportClause,
    AnyJsExportDefaultDeclaration, AnyJsExportNamedSpecifier, AnyJsExpression, AnyTsType, JsExport,
    JsExportNamedClause, JsIdentifierExpression, JsLiteralExportName, JsReferenceIdentifier,
    JsSyntaxToken, TsEnumDeclaration, TsIdentifierBinding,
};

declare_node_union! {
    pub AnyIdentifier = JsReferenceIdentifier | AnyJsBindingPattern | AnyJsBinding | JsIdentifierExpression | JsLiteralExportName | TsIdentifierBinding
}

declare_node_union! {
    pub AnyJsExported = AnyJsExpression | AnyJsExportClause | AnyTsType | TsEnumDeclaration
}

#[derive(Clone)]
pub struct ExportedItem {
    pub identifier: AnyIdentifier,
    pub exported: Option<AnyJsExported>,
}

impl JsExport {
    /// Returns the pair of id and entity of the exported object
    pub fn get_exported_items(&self) -> Vec<ExportedItem> {
        self.export_clause()
            .ok()
            .and_then(|export_clause| match export_clause {
                // export const x = 100;
                AnyJsExportClause::AnyJsDeclarationClause(declaration_clause) => {
                    match declaration_clause {
                        AnyJsDeclarationClause::JsVariableDeclarationClause(
                            variable_declaration_clause,
                        ) => variable_declaration_clause.declaration().ok().map(
                            |variable_declaration| {
                                variable_declaration
                                    .declarators()
                                    .into_iter()
                                    .filter_map(|declarator| {
                                        let declarator = declarator.ok()?;
                                        let identifier = declarator.id().ok()?;
                                        let initializer = declarator
                                            .initializer()
                                            .and_then(|init| init.expression().ok());
                                        Some(ExportedItem {
                                            identifier: AnyIdentifier::AnyJsBindingPattern(
                                                identifier,
                                            ),
                                            exported: initializer
                                                .map(AnyJsExported::AnyJsExpression),
                                        })
                                    })
                                    .collect()
                            },
                        ),
                        AnyJsDeclarationClause::JsFunctionDeclaration(
                            function_declaration_clause,
                        ) => function_declaration_clause.id().ok().map(|function_id| {
                            vec![ExportedItem {
                                identifier: AnyIdentifier::AnyJsBinding(function_id),
                                exported: None,
                            }]
                        }),
                        AnyJsDeclarationClause::TsEnumDeclaration(ts_enum_declaration) => {
                            ts_enum_declaration.id().ok().map(|enum_id| {
                                vec![ExportedItem {
                                    identifier: AnyIdentifier::AnyJsBinding(enum_id),
                                    exported: Some(AnyJsExported::TsEnumDeclaration(
                                        ts_enum_declaration,
                                    )),
                                }]
                            })
                        }
                        AnyJsDeclarationClause::TsTypeAliasDeclaration(
                            ts_type_alias_declaration,
                        ) => ts_type_alias_declaration.binding_identifier().ok().map(
                            |type_alias_id| {
                                vec![ExportedItem {
                                    identifier: AnyIdentifier::TsIdentifierBinding(type_alias_id),
                                    exported: ts_type_alias_declaration
                                        .ty()
                                        .ok()
                                        .map(AnyJsExported::AnyTsType),
                                }]
                            },
                        ),
                        _ => None,
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
                            vec![ExportedItem {
                                identifier: AnyIdentifier::AnyJsBinding(any_js_binding),
                                exported: None,
                            }]
                        })
                }
                // export default x;
                AnyJsExportClause::JsExportDefaultExpressionClause(clause) => clause
                    .expression()
                    .ok()
                    .and_then(|expression| match expression {
                        AnyJsExpression::JsIdentifierExpression(identifier) => {
                            Some(vec![ExportedItem {
                                identifier: AnyIdentifier::JsIdentifierExpression(identifier),
                                exported: None,
                            }])
                        }
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
}

impl AnyJsExportNamedSpecifier {
    /// Type token of the export specifier.
    ///
    /// ```ts
    /// export { type X }
    ///          ^^^^
    /// ```
    pub fn type_token(&self) -> Option<JsSyntaxToken> {
        match self {
            Self::JsExportNamedShorthandSpecifier(specifier) => specifier.type_token(),
            Self::JsExportNamedSpecifier(specifier) => specifier.type_token(),
        }
    }

    /// Returns the export clause that includes this specifier.
    pub fn export_named_clause(&self) -> Option<JsExportNamedClause> {
        JsExportNamedClause::cast(self.syntax().grand_parent()?)
    }

    /// Returns `true` if this specifier or its export clause has **only** a type modifier.
    pub fn exports_only_types(&self) -> bool {
        self.type_token().is_some()
            || self
                .export_named_clause()
                .and_then(|x| x.type_token())
                .is_some()
    }

    /// Returns the local name of the export.
    ///
    /// ## Examples
    ///
    /// ```
    /// use biome_js_syntax::{AnyJsExportNamedSpecifier, T};
    /// use biome_js_factory::make;
    ///
    /// let specifier = make::js_export_named_shorthand_specifier(
    ///     make::js_reference_identifier(make::ident("a"))
    /// ).with_type_token(make::token(T![type])).build();
    /// let export = AnyJsExportNamedSpecifier::from(specifier.clone());
    ///
    /// assert_eq!(export.local_name(), specifier.name());
    ///
    /// let specifier = make::js_export_named_specifier(
    ///     make::js_reference_identifier(make::ident("a")),
    ///     make::token(T![as]),
    ///     make::js_literal_export_name(make::ident("b")),
    /// ).build();
    /// let export = AnyJsExportNamedSpecifier::from(specifier.clone());
    ///
    /// assert_eq!(export.local_name(), specifier.local_name());
    /// ```
    pub fn local_name(&self) -> SyntaxResult<JsReferenceIdentifier> {
        match self {
            Self::JsExportNamedShorthandSpecifier(specifier) => specifier.name(),
            Self::JsExportNamedSpecifier(specifier) => specifier.local_name(),
        }
    }

    pub fn with_type_token(self, type_token: Option<JsSyntaxToken>) -> Self {
        match self {
            Self::JsExportNamedShorthandSpecifier(specifier) => {
                specifier.with_type_token(type_token).into()
            }
            Self::JsExportNamedSpecifier(specifier) => specifier.with_type_token(type_token).into(),
        }
    }
}
