
data Optional a = Nada
                | Only a
                deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
    (<>) Nada     Nada     = Nada
    (<>) (Only x) Nada     = Only x
    (<>) Nada     (Only x) = Only x
    (<>) (Only x) (Only y) = Only $ x <> y

instance Monoid a => Monoid (Optional a) where
    mempty = Nada

