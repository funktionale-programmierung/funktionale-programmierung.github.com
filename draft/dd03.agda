module DD03 where

data Nat : Set where
  zero : Nat
  suc : Nat -> Nat

one : Nat
one = suc zero

two : Nat
two = suc one

three : Nat
three = suc two

+ : Nat -> Nat -> Nat
+ zero m = m
+ (suc n) m = suc (+ n m)

* : Nat -> Nat -> Nat
* zero m = zero
* (suc n) m = (+ m (* n m))

data Bin : Set where
  0b : Bin
  1b : Bin
  at-0 : Bin -> Bin
  at-1 : Bin -> Bin

10b : Bin
10b = at-0 1b

11b : Bin
11b = at-1 1b

100b : Bin
100b = at-0 (at-0 1b)

inc : Bin -> Bin
inc 0b = 1b
inc 1b = 10b
inc (at-0 x) = at-1 x
inc (at-1 x) = at-0 (inc x)


μ : Bin -> Nat
μ 0b = zero
μ 1b = one
μ (at-0 x) = (* two (μ x))
μ (at-1 x) = (+ (* two (μ x)) one)

data === (x : Nat) : Nat -> Set where
  refl : (=== x x)

data ===₂ (A : Set) (x : A) : A -> Set where
  refl : (===₂ A x x)

yes : (=== (+ one two) three)
yes = refl

yes2 : (===₂ Bin (inc 11b) 100b)
yes2 = refl

-- no : (=== (+ one one) three)
-- no = refl

trans : (x : Nat) -> (y : Nat) -> (z : Nat) -> (=== x y) -> (=== y z) -> (=== x z)
trans x y z refl refl = refl

trans2 : {x : Nat} -> {y : Nat} -> {z : Nat} -> (=== x y) -> (=== y z) -> (=== x z)
trans2 refl refl = refl

cong : (f : Nat -> Nat) -> (x : Nat) -> (y : Nat) -> (=== x y) -> (=== (f x) (f y))
cong f x y refl = refl

cong2 : {x : Nat} -> {y : Nat} -> (f : Nat -> Nat) -> (=== x y) -> (=== (f x) (f y))
cong2 f refl = refl

zeroneutral : (x : Nat) -> (=== (+ x zero) x)
zeroneutral zero = refl
zeroneutral (suc x) = cong suc (+ x zero) x (zeroneutral x)

+assoc : (x : Nat) -> (y : Nat) -> (z : Nat) -> (=== (+ x (+ y z)) (+ (+ x y) z))
+assoc zero y z = refl
+assoc (suc x) y z = cong suc (+ x (+ y z)) (+ (+ x y) z) (+assoc x y z)

sym : (x : Nat) -> (y : Nat) -> (=== x y) -> (=== y x)
sym x y refl = refl

sym2 : {x : Nat} -> {y : Nat} -> (=== x y) -> (=== y x)
sym2 refl = refl

+suclr : (x : Nat) -> (y : Nat) -> (=== (+ x (suc y)) (+ (suc x) y))
+suclr zero y = refl
+suclr (suc x) y = cong suc (+ x (suc y)) (+ (suc x) y) (+suclr x y)

+commut : (x : Nat) -> (y : Nat) -> (=== (+ x y) (+ y x))
+commut zero y = sym (+ y zero) y (zeroneutral y)
+commut (suc x) zero = cong suc (+ x zero) x (zeroneutral x)
+commut (suc x) (suc y) = cong
                            suc
                            (+ x (suc y))
                            (+ y (suc x))
                            (trans
                              (+ x (suc y))
                              (+ (suc x) y)
                              (+ y (suc x))
                              (+suclr x y)
                              (+commut (suc x) y))

+commut2 : (x : Nat) -> (y : Nat) -> (=== (+ x y) (+ y x))
+commut2 zero y = sym2 (zeroneutral y)
+commut2 (suc x) zero = cong2 suc (zeroneutral x)
+commut2 (suc x) (suc y) = cong2
                             suc
                             (trans2
                               (+suclr _ _)
                               (+commut2 (suc x) y))


-- inc===+1 : (x : Bin) -> (=== (𝛍 (inc x)) (+ (𝛍 x) one))
-- inc===+1 0b = refl
-- inc===+1 1b = refl
-- inc===+1 (at-0 x) = refl
-- inc===+1 (at-1 x) = begin ? ≡⟨ ? ⟩ ? ∎ where open ≡-Reasoning

inc0b : (=== (μ (inc 0b)) (+ (μ 0b) one))
inc0b = refl

inc===+1 : {x : Bin} -> === (μ (inc x)) (+ (μ x) one)
inc===+1 {0b} = refl
inc===+1 {1b} = refl
inc===+1 {at-0 x} = refl
inc===+1 {at-1 x} = trans2 (+assoc (μ (inc x)) (μ (inc x)) zero)
                           (trans2 (zeroneutral (+ (μ (inc x)) (μ (inc x))))
                                   (trans2 (cong2 (λ y → (+ y (μ (inc x))))
                                                  (inc===+1 {x}))
                                           (trans2 (cong2 (λ y → (+ (+ (μ x) one) y))
                                                          (inc===+1 {x}))
                                                   (trans2 (+assoc (+ (μ x) one) (μ x) one)
                                                           (cong2 (λ y → (+ y one))
                                                                  (trans2 (+commut2 (+ (μ x) one) (μ x))
                                                                          (trans2 (+assoc (μ x) (μ x) one)
                                                                                  (cong2 (λ y → (+ y one))
                                                                                         (cong2 (λ y → (+ (μ x) y))
                                                                                                (sym2 (zeroneutral (μ x))))))))))))
