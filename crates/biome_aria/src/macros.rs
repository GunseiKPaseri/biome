#[macro_export]
macro_rules! define_role {
    ( $( #[doc = $doc:literal] )+ $id:ident {
        PROPS: $p_value:expr,
        ROLES: $r_value:expr,
    }) => {
        $( #[doc = $doc] )*
        #[derive(Debug)]
        struct $id;

        impl $id {
            const PROPS: &'static [(&'static str, bool)] = &$p_value;
            const ROLES: &'static [&'static str] = &$r_value;
        }

        impl $crate::AriaRoleDefinition for $id {
            fn properties(&self) -> Iter<(&str, bool)> {
                $id::PROPS.iter()
            }

            fn roles(&self) -> Iter<&str> {
                $id::ROLES.iter()
            }
        }
    };
    ( $( #[doc = $doc:literal] )+ $id:ident {
        PROPS: $p_value:expr,
        ROLES: $r_value:expr,
        CONCEPTS: $c_value:expr,
    }) => {
        $( #[doc = $doc] )*
        #[derive(Debug)]
        struct $id;

        impl $id {
            const PROPS: &'static [(&'static str, bool)] = &$p_value;
            const ROLES: &'static [&'static str] = &$r_value;
            const CONCEPTS: &'static [(&'static str, &'static [(&'static str, &'static str)])] =
                $c_value;
        }

        impl $crate::AriaRoleDefinition for $id {
            fn properties(&self) -> Iter<(&str, bool)> {
                $id::PROPS.iter()
            }

            fn roles(&self) -> Iter<&str> {
                $id::ROLES.iter()
            }
        }

        impl AriaRoleDefinitionWithConcepts for $id {
            fn concepts_by_role<'a>(&self) -> ElementsAndAttributes<'a> {
                Some(Self::CONCEPTS.iter())
            }
        }
    };
}

#[macro_export]
macro_rules! define_property {
    ( $id:ident {
        PROPERTY_TYPE: $property_type:literal,
        VALUES: $values:expr,
    }) => {
        #[derive(Debug)]
        struct $id;

        impl $id {
            const PROPERTY_TYPE: &'static str = &$property_type;
            const VALUES: &'static [&'static str] = &$values;
        }

        impl AriaPropertyDefinition for $id {
            fn values(&self) -> Iter<&'static str> {
                $id::VALUES.iter()
            }

            fn property_type(&self) -> $crate::AriaPropertyTypeEnum {
                // SAFETY: PROPERTY_TYPE is internal and should not contain extraneous properties
                $crate::AriaPropertyTypeEnum::from_str($id::PROPERTY_TYPE).unwrap()
            }
        }
    };
}
