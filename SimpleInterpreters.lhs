data AE =
    Val int
	| Add AE AE
	| Sub AE AE
	  deriving (Eq,Show)