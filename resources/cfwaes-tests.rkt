(test (eval-cfwaes '{with {y 0}
                       {with {inc {fun x {+ x 1}}}
                         {seq {seq {assign y {inc y}}
                                   {assign y {inc y}}}
                              {seq {assign y {inc y}}
                                   {assign y {inc y}}}}}}) (numV 4))

(test (eval-cfwaes '{with {y 1}
                       {with {inc {fun x {+ x y}}}
                         {inc 3}}}) (numV 4))

(test (eval-cfwaes '{with {y 1}
                       {with {inc {fun x {+ x y}}}
                         {seq {assign y 2} {inc 3}}}}) (numV 5))

(test (eval-cfwaes '{with {y 1}
                       {with {inc {seq {assign y 2} {fun x {+ x y}}}}
                         {inc 3}}}) (numV 5))

(test (eval-cfwaes '{with {x 3}
                       {seq x {assign x {+ x 1}}}}) (numV 4))

(test (eval-cfwaes '{with {x 3}
                       {seq
                         {assign x {+ x 1}} {assign x {+ x 1}}}}) (numV 5))

(test (eval-cfwaes '{with {x 3}
                       {seq
                        {seq
                         {assign x {+ x 1}} {assign x {+ x 1}}}
                        {assign x {+ x 1}}}}) (numV 6))