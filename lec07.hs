import Control.Monad

data Tree a = Node (Tree a) a (Tree a)
            | Empty
              deriving (Show)

zipTree1 :: (a -> b -> c) -> Tree a -> Tree b -> Maybe (Tree c)
zipTree1 _ Node{} Empty = Nothing
zipTree1 _ Empty Node{} = Nothing
zipTree1 _ Empty Empty  = Just Empty
zipTree1 f (Node l1 x r1) (Node l2 y r2) =
    case zipTree1 f l1 l2 of
        Nothing -> Nothing
        Just l  -> case zipTree1 f r1 r2 of
                    Nothing -> Nothing
                    Just r  -> Just $ Node l (f x y) r

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe mx f = case mx of
    Nothing -> Nothing
    Just x  -> Just f x

zipTree2 :: (a -> b -> c) -> Tree a -> Tree b -> Maybe (Tree c)
zipTree2 _ Node{} Empty = Nothing
zipTree2 _ Empty Node{} = Nothing
zipTree2 _ Empty Empty  = Just Empty
zipTree2 f (Node l1 x r1) (Node l2 y r2) =
    bindMaybe (zipTree2 f l1 l2) $ \l ->
        bindMaybe (zipTree2 f r1 r2) $ \r ->
            Just (Node l (f x y) r)

instance Monad Maybe where
    return = Just
    Nothing >>= _ = Nothing
    Just x >>= f = f x

zipTree3 :: (a -> b -> c) -> Tree a -> Tree b -> Maybe (Tree c)
zipTree3 _ Node{} Empty = Nothing
zipTree3 _ Empty Node{} = Nothing
zipTree3 _ Empty Empty  = Just Empty
zipTree3 f (Node l1 x r1) (Node l2 y r2) =
    zipTree3 f l1 l2 >>= \l ->
        zipTree3 f r1 r2 >>= \r ->
            Just (Node l (f x y) r)

zipTree :: (a -> b -> c) -> Tree a -> Tree b -> Maybe (Tree c)
zipTree _ Node{} Empty = Nothing
zipTree _ Empty Node{} = Nothing
zipTree _ Empty Empty  = Just Empty
zipTree f (Node l1 x r1) (Node l2 y r2) = do
    l <- zipTree f l1 l2
    r <- zipTree f r1 r2
    return $ Node l (f x y) r




