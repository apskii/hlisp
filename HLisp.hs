


eval = runEvalT ∘ evalT

evalString = either (error ∘ pretty) (pretty ∘ last)
           ∘ runEvalT ∘ sequence ∘ map evalT ∘ parse form