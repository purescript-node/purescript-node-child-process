module Test.Internal (makeNonCancelerAff) where 

import Prelude (Unit, (*>), pure)
import Data.Either (Either)
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Exception (Error)

makeNonCancelerAff :: forall a b. ((Either Error a -> Effect Unit) -> Effect b) -> Aff a
makeNonCancelerAff handler = makeAff (\cb -> handler cb *> pure nonCanceler)
