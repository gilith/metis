val large_problem_quotation =
 `(!x. x = x) /\
  (!x y. ~x = y \/ y = x) /\
  (!x y z. ~x = y \/ ~y = z \/ x = z) /\
  (!x y. ~member x y \/ little_set x) /\
  (!x y. little_set (f1 x y) \/ x = y) /\
  (!x y. member (f1 x y) x \/ member (f1 x y) y \/ x = y) /\
  (!x y. ~member (f1 x y) x \/ ~member (f1 x y) y \/ x = y) /\
  (!x y z. ~member x (non_ordered_pair y z) \/ x = y \/ x = z) /\
  (!x y z. member x (non_ordered_pair y z) \/ ~little_set x \/ ~x = y) /\
  (!x y z. member x (non_ordered_pair y z) \/ ~little_set x \/ ~x = z) /\
  (!x y. little_set (non_ordered_pair x y)) /\
  (!x. singleton_set x = non_ordered_pair x x) /\
  (!x y.
     ordered_pair x y = non_ordered_pair (singleton_set x)
     (non_ordered_pair x y)) /\
  (!x. ~ordered_pair_predicate x \/ little_set (f2 x)) /\
  (!x. ~ordered_pair_predicate x \/ little_set (f3 x)) /\
  (!x. ~ordered_pair_predicate x \/ x = ordered_pair (f2 x) (f3 x)) /\
  (!x y z.
     ordered_pair_predicate x \/ ~little_set y \/ ~little_set z \/
     ~x = ordered_pair y z) /\
  (!x y. ~member x (first y) \/ little_set (f4 x y)) /\
  (!x y. ~member x (first y) \/ little_set (f5 x y)) /\
  (!x y. ~member x (first y) \/ y = ordered_pair (f4 x y) (f5 x y)) /\
  (!x y. ~member x (first y) \/ member x (f4 x y)) /\
  (!x y z v.
     member x (first y) \/ ~little_set z \/ ~little_set v \/
     ~y = ordered_pair z v \/ ~member x z) /\
  (!x y. ~member x (second y) \/ little_set (f6 x y)) /\
  (!x y. ~member x (second y) \/ little_set (f7 x y)) /\
  (!x y. ~member x (second y) \/ y = ordered_pair (f6 x y) (f7 x y)) /\
  (!x y. ~member x (second y) \/ member x (f7 x y)) /\
  (!x y z v.
     member x (second y) \/ ~little_set z \/ ~little_set v \/
     ~y = ordered_pair z v \/ ~member x v) /\
  (!x. ~member x estin \/ ordered_pair_predicate x) /\
  (!x. ~member x estin \/ member (first x) (second x)) /\
  (!x.
     member x estin \/ ~little_set x \/ ~ordered_pair_predicate x \/
     ~member (first x) (second x)) /\
  (!x y z. ~member x (intersection y z) \/ member x y) /\
  (!x y z. ~member x (intersection y z) \/ member x z) /\
  (!x y z. member x (intersection y z) \/ ~member x y \/ ~member x z) /\
  (!x y. ~member x (complement y) \/ ~member x y) /\
  (!x y. member x (complement y) \/ ~little_set x \/ member x y) /\
  (!x y.
     union x y = complement (intersection (complement x) (complement y))) /\
  (!x y. ~member x (domain_of y) \/ ordered_pair_predicate (f8 x y)) /\
  (!x y. ~member x (domain_of y) \/ member (f8 x y) y) /\
  (!x y. ~member x (domain_of y) \/ x = first (f8 x y)) /\
  (!x y z.
     member x (domain_of y) \/ ~little_set x \/ ~ordered_pair_predicate z \/
     ~member z y \/ ~x = first z) /\
  (!x y z. ~member x (cross_product y z) \/ ordered_pair_predicate x) /\
  (!x y z. ~member x (cross_product y z) \/ member (first x) y) /\
  (!x y z. ~member x (cross_product y z) \/ member (second x) z) /\
  (!x y z.
     member x (cross_product y z) \/ ~little_set x \/
     ~ordered_pair_predicate x \/ ~member (first x) y \/
     ~member (second x) z) /\
  (!x y. ~member x (converse y) \/ ordered_pair_predicate x) /\
  (!x y.
     ~member x (converse y) \/
     member (ordered_pair (second x) (first x)) y) /\
  (!x y.
     member x (converse y) \/ ~little_set x \/ ~ordered_pair_predicate x \/
     ~member (ordered_pair (second x) (first x)) y) /\
  (!x y. ~member x (rotate_right y) \/ little_set (f9 x y)) /\
  (!x y. ~member x (rotate_right y) \/ little_set (f10 x y)) /\
  (!x y. ~member x (rotate_right y) \/ little_set (f11 x y)) /\
  (!x y.
     ~member x (rotate_right y) \/
     x = ordered_pair (f9 x y) (ordered_pair (f10 x y) (f11 x y))) /\
  (!x y.
     ~member x (rotate_right y) \/
     member (ordered_pair (f10 x y) (ordered_pair (f11 x y) (f9 x y))) y) /\
  (!x y z v w.
     member x (rotate_right y) \/ ~little_set x \/ ~little_set z \/
     ~little_set v \/ ~little_set w \/
     ~x = ordered_pair z (ordered_pair v w) \/
     ~member (ordered_pair v (ordered_pair w z)) y) /\
  (!x y. ~member x (flip_range_of y) \/ little_set (f12 x y)) /\
  (!x y. ~member x (flip_range_of y) \/ little_set (f13 x y)) /\
  (!x y. ~member x (flip_range_of y) \/ little_set (f14 x y)) /\
  (!x y.
     ~member x (flip_range_of y) \/
     x = ordered_pair (f12 x y) (ordered_pair (f13 x y) (f14 x y))) /\
  (!x y.
     ~member x (flip_range_of y) \/
     member (ordered_pair (f12 x y) (ordered_pair (f14 x y) (f13 x y))) y) /\
  (!x y z v w.
     member x (flip_range_of y) \/ ~little_set x \/ ~little_set z \/
     ~little_set v \/ ~little_set w \/
     ~x = ordered_pair z (ordered_pair v w) \/
     ~member (ordered_pair z (ordered_pair w v)) y) /\
  (!x. successor x = union x (singleton_set x)) /\
  (!x. ~member x empty_set) /\
  (!x. member x universal_set \/ ~little_set x) /\ little_set infinity /\
  member empty_set infinity /\
  (!x. ~member x infinity \/ member (successor x) infinity) /\
  (!x y. ~member x (sigma y) \/ member (f16 x y) y) /\
  (!x y. ~member x (sigma y) \/ member x (f16 x y)) /\
  (!x y z. member x (sigma y) \/ ~member z y \/ ~member x z) /\
  (!x. ~little_set x \/ little_set (sigma x)) /\
  (!x y z. ~subset x y \/ ~member z x \/ member z y) /\
  (!x y. subset x y \/ member (f17 x y) x) /\
  (!x y. subset x y \/ ~member (f17 x y) y) /\
  (!x y. ~proper_subset x y \/ subset x y) /\
  (!x y. ~proper_subset x y \/ ~x = y) /\
  (!x y. proper_subset x y \/ ~subset x y \/ x = y) /\
  (!x y. ~member x (powerset y) \/ subset x y) /\
  (!x y. member x (powerset y) \/ ~little_set x \/ ~subset x y) /\
  (!x. ~little_set x \/ little_set (powerset x)) /\
  (!x y. ~relation x \/ ~member y x \/ ordered_pair_predicate y) /\
  (!x. relation x \/ member (f18 x) x) /\
  (!x. relation x \/ ~ordered_pair_predicate (f18 x)) /\
  (!x y z v.
     ~single_valued_set x \/ ~little_set y \/ ~little_set z \/
     ~little_set v \/ ~member (ordered_pair y z) x \/
     ~member (ordered_pair y v) x \/ z = v) /\
  (!x. single_valued_set x \/ little_set (f19 x)) /\
  (!x. single_valued_set x \/ little_set (f20 x)) /\
  (!x. single_valued_set x \/ little_set (f21 x)) /\
  (!x. single_valued_set x \/ member (ordered_pair (f19 x) (f20 x)) x) /\
  (!x. single_valued_set x \/ member (ordered_pair (f19 x) (f21 x)) x) /\
  (!x. single_valued_set x \/ ~f20 x = f21 x) /\
  (!x. ~function x \/ relation x) /\
  (!x. ~function x \/ single_valued_set x) /\
  (!x. function x \/ ~relation x \/ ~single_valued_set x) /\
  (!x y z. ~member x (image y z) \/ ordered_pair_predicate (f22 x y z)) /\
  (!x y z. ~member x (image y z) \/ member (f22 x y z) z) /\
  (!x y z. ~member x (image y z) \/ member (first (f22 x y z)) y) /\
  (!x y z. ~member x (image y z) \/ second (f22 x y z) = x) /\
  (!x y z v.
     member x (image y z) \/ ~little_set x \/ ~ordered_pair_predicate v \/
     ~member v z \/ ~member (first v) y \/ ~second v = x) /\
  (!x y. ~little_set x \/ ~function y \/ little_set (image x y)) /\
  (!x y z. ~disjoint x y \/ ~member z x \/ ~member z y) /\
  (!x y. disjoint x y \/ member (f23 x y) x) /\
  (!x y. disjoint x y \/ member (f23 x y) y) /\
  (!x. x = empty_set \/ member (f24 x) x) /\
  (!x. x = empty_set \/ disjoint (f24 x) x) /\ function f25 /\
  (!x. ~little_set x \/ x = empty_set \/ member (f26 x) x) /\
  (!x.
     ~little_set x \/ x = empty_set \/
     member (ordered_pair x (f26 x)) f25) /\
  (!x y. ~member x (range_of y) \/ ordered_pair_predicate (f27 x y)) /\
  (!x y. ~member x (range_of y) \/ member (f27 x y) y) /\
  (!x y. ~member x (range_of y) \/ x = second (f27 x y)) /\
  (!x y z.
     member x (range_of y) \/ ~little_set x \/ ~ordered_pair_predicate z \/
     ~member z y \/ ~x = second z) /\
  (!x. ~member x identity_relation \/ ordered_pair_predicate x) /\
  (!x. ~member x identity_relation \/ first x = second x) /\
  (!x.
     member x identity_relation \/ ~little_set x \/
     ~ordered_pair_predicate x \/ ~first x = second x) /\
  (!x y. restrict x y = intersection x (cross_product y universal_set)) /\
  (!x. ~one_to_one_function x \/ function x) /\
  (!x. ~one_to_one_function x \/ function (converse x)) /\
  (!x. one_to_one_function x \/ ~function x \/ ~function (converse x)) /\
  (!x y z. ~member x (apply y z) \/ ordered_pair_predicate (f28 x y z)) /\
  (!x y z. ~member x (apply y z) \/ member (f28 x y z) y) /\
  (!x y z. ~member x (apply y z) \/ first (f28 x y z) = z) /\
  (!x y z. ~member x (apply y z) \/ member x (second (f28 x y z))) /\
  (!x y z v.
     member x (apply y z) \/ ~ordered_pair_predicate v \/ ~member v y \/
     ~first v = z \/ ~member x (second v)) /\
  (!x y z. apply_to_two_arguments x y z = apply x (ordered_pair y z)) /\
  (!x y z. ~maps x y z \/ function x) /\
  (!x y z. ~maps x y z \/ domain_of x = y) /\
  (!x y z. ~maps x y z \/ subset (range_of x) z) /\
  (!x y z.
     maps x y z \/ ~function x \/ ~domain_of x = y \/
     ~subset (range_of x) z) /\ (!x y. ~closed x y \/ little_set x) /\
  (!x y. ~closed x y \/ little_set y) /\
  (!x y. ~closed x y \/ maps y (cross_product x x) x) /\
  (!x y.
     closed x y \/ ~little_set x \/ ~little_set y \/
     ~maps y (cross_product x x) x) /\
  (!x y z. ~member x (compose y z) \/ little_set (f29 x y z)) /\
  (!x y z. ~member x (compose y z) \/ little_set (f30 x y z)) /\
  (!x y z. ~member x (compose y z) \/ little_set (f31 x y z)) /\
  (!x y z.
     ~member x (compose y z) \/ x = ordered_pair (f29 x y z) (f30 x y z)) /\
  (!x y z.
     ~member x (compose y z) \/
     member (ordered_pair (f29 x y z) (f31 x y z)) y) /\
  (!x y z.
     ~member x (compose y z) \/
     member (ordered_pair (f31 x y z) (f30 x y z)) z) /\
  (!x y z x' y' z'.
     member x (compose y z) \/ ~little_set x \/ ~little_set x' \/
     ~little_set y' \/ ~little_set z' \/ ~x = ordered_pair x' y' \/
     ~member (ordered_pair x' z') y \/ ~member (ordered_pair z' y') z) /\
  (!x y z v w. ~homomorphism x y z v w \/ closed y z) /\
  (!x y z v w. ~homomorphism x y z v w \/ closed v w) /\
  (!x y z v w. ~homomorphism x y z v w \/ maps x y v) /\
  (!x y z v w x' y'.
     ~homomorphism x y z v w \/ ~member x' y \/ ~member y' y \/
     apply x (apply_to_two_arguments z x' y') = apply_to_two_arguments w
     (apply x x') (apply x y')) /\
  (!x y z v w.
     homomorphism x y z v w \/ ~closed y z \/ ~closed v w \/ ~maps x y v \/
     member (f32 x y z v w) y) /\
  (!x y z v w.
     homomorphism x y z v w \/ ~closed y z \/ ~closed v w \/ ~maps x y v \/
     member (f33 x y z v w) y) /\
  (!x y z v w.
     homomorphism x y z v w \/ ~closed y z \/ ~closed v w \/ ~maps x y v \/
     ~apply x (apply_to_two_arguments z (f32 x y z v w) (f33 x y z v w)) =
      apply_to_two_arguments w (apply x (f32 x y z v w))
      (apply x (f33 x y z v w))) /\ (!x y z. ~x = y \/ f1 x z = f1 y z) /\
  (!x y z. ~x = y \/ f1 z x = f1 z y) /\ (!x y. ~x = y \/ f2 x = f2 y) /\
  (!x y. ~x = y \/ f3 x = f3 y) /\ (!x y z. ~x = y \/ f4 x z = f4 y z) /\
  (!x y z. ~x = y \/ f4 z x = f4 z y) /\
  (!x y z. ~x = y \/ f5 x z = f5 y z) /\
  (!x y z. ~x = y \/ f5 z x = f5 z y) /\
  (!x y z. ~x = y \/ f6 x z = f6 y z) /\
  (!x y z. ~x = y \/ f6 z x = f6 z y) /\
  (!x y z. ~x = y \/ f7 x z = f7 y z) /\
  (!x y z. ~x = y \/ f7 z x = f7 z y) /\
  (!x y z. ~x = y \/ f8 x z = f8 y z) /\
  (!x y z. ~x = y \/ f8 z x = f8 z y) /\
  (!x y z. ~x = y \/ f9 x z = f9 y z) /\
  (!x y z. ~x = y \/ f9 z x = f9 z y) /\
  (!x y z. ~x = y \/ f10 x z = f10 y z) /\
  (!x y z. ~x = y \/ f10 z x = f10 z y) /\
  (!x y z. ~x = y \/ f11 x z = f11 y z) /\
  (!x y z. ~x = y \/ f11 z x = f11 z y) /\
  (!x y z. ~x = y \/ f12 x z = f12 y z) /\
  (!x y z. ~x = y \/ f12 z x = f12 z y) /\
  (!x y z. ~x = y \/ f13 x z = f13 y z) /\
  (!x y z. ~x = y \/ f13 z x = f13 z y) /\
  (!x y z. ~x = y \/ f14 x z = f14 y z) /\
  (!x y z. ~x = y \/ f14 z x = f14 z y) /\
  (!x y z. ~x = y \/ f16 x z = f16 y z) /\
  (!x y z. ~x = y \/ f16 z x = f16 z y) /\
  (!x y z. ~x = y \/ f17 x z = f17 y z) /\
  (!x y z. ~x = y \/ f17 z x = f17 z y) /\ (!x y. ~x = y \/ f18 x = f18 y) /\
  (!x y. ~x = y \/ f19 x = f19 y) /\ (!x y. ~x = y \/ f20 x = f20 y) /\
  (!x y. ~x = y \/ f21 x = f21 y) /\
  (!x y z v. ~x = y \/ f22 x z v = f22 y z v) /\
  (!x y z v. ~x = y \/ f22 z x v = f22 z y v) /\
  (!x y z v. ~x = y \/ f22 z v x = f22 z v y) /\
  (!x y z. ~x = y \/ f23 x z = f23 y z) /\
  (!x y z. ~x = y \/ f23 z x = f23 z y) /\ (!x y. ~x = y \/ f24 x = f24 y) /\
  (!x y. ~x = y \/ f26 x = f26 y) /\ (!x y z. ~x = y \/ f27 x z = f27 y z) /\
  (!x y z. ~x = y \/ f27 z x = f27 z y) /\
  (!x y z v. ~x = y \/ f28 x z v = f28 y z v) /\
  (!x y z v. ~x = y \/ f28 z x v = f28 z y v) /\
  (!x y z v. ~x = y \/ f28 z v x = f28 z v y) /\
  (!x y z v. ~x = y \/ f29 x z v = f29 y z v) /\
  (!x y z v. ~x = y \/ f29 z x v = f29 z y v) /\
  (!x y z v. ~x = y \/ f29 z v x = f29 z v y) /\
  (!x y z v. ~x = y \/ f30 x z v = f30 y z v) /\
  (!x y z v. ~x = y \/ f30 z x v = f30 z y v) /\
  (!x y z v. ~x = y \/ f30 z v x = f30 z v y) /\
  (!x y z v. ~x = y \/ f31 x z v = f31 y z v) /\
  (!x y z v. ~x = y \/ f31 z x v = f31 z y v) /\
  (!x y z v. ~x = y \/ f31 z v x = f31 z v y) /\
  (!x y z x' y' z'. ~x = y \/ f32 x z x' y' z' = f32 y z x' y' z') /\
  (!x y z x' y' z'. ~x = y \/ f32 z x x' y' z' = f32 z y x' y' z') /\
  (!x y z x' y' z'. ~x = y \/ f32 z x' x y' z' = f32 z x' y y' z') /\
  (!x y z x' y' z'. ~x = y \/ f32 z x' y' x z' = f32 z x' y' y z') /\
  (!x y z x' y' z'. ~x = y \/ f32 z x' y' z' x = f32 z x' y' z' y) /\
  (!x y z x' y' z'. ~x = y \/ f33 x z x' y' z' = f33 y z x' y' z') /\
  (!x y z x' y' z'. ~x = y \/ f33 z x x' y' z' = f33 z y x' y' z') /\
  (!x y z x' y' z'. ~x = y \/ f33 z x' x y' z' = f33 z x' y y' z') /\
  (!x y z x' y' z'. ~x = y \/ f33 z x' y' x z' = f33 z x' y' y z') /\
  (!x y z x' y' z'. ~x = y \/ f33 z x' y' z' x = f33 z x' y' z' y) /\
  (!x y z. ~x = y \/ apply x z = apply y z) /\
  (!x y z. ~x = y \/ apply z x = apply z y) /\
  (!x y z v.
     ~x = y \/
     apply_to_two_arguments x z v = apply_to_two_arguments y z v) /\
  (!x y z v.
     ~x = y \/
     apply_to_two_arguments z x v = apply_to_two_arguments z y v) /\
  (!x y z v.
     ~x = y \/
     apply_to_two_arguments z v x = apply_to_two_arguments z v y) /\
  (!x y. ~x = y \/ complement x = complement y) /\
  (!x y z. ~x = y \/ compose x z = compose y z) /\
  (!x y z. ~x = y \/ compose z x = compose z y) /\
  (!x y. ~x = y \/ converse x = converse y) /\
  (!x y z. ~x = y \/ cross_product x z = cross_product y z) /\
  (!x y z. ~x = y \/ cross_product z x = cross_product z y) /\
  (!x y. ~x = y \/ domain_of x = domain_of y) /\
  (!x y. ~x = y \/ first x = first y) /\
  (!x y. ~x = y \/ flip_range_of x = flip_range_of y) /\
  (!x y z. ~x = y \/ image x z = image y z) /\
  (!x y z. ~x = y \/ image z x = image z y) /\
  (!x y z. ~x = y \/ intersection x z = intersection y z) /\
  (!x y z. ~x = y \/ intersection z x = intersection z y) /\
  (!x y z. ~x = y \/ non_ordered_pair x z = non_ordered_pair y z) /\
  (!x y z. ~x = y \/ non_ordered_pair z x = non_ordered_pair z y) /\
  (!x y z. ~x = y \/ ordered_pair x z = ordered_pair y z) /\
  (!x y z. ~x = y \/ ordered_pair z x = ordered_pair z y) /\
  (!x y. ~x = y \/ powerset x = powerset y) /\
  (!x y. ~x = y \/ range_of x = range_of y) /\
  (!x y z. ~x = y \/ restrict x z = restrict y z) /\
  (!x y z. ~x = y \/ restrict z x = restrict z y) /\
  (!x y. ~x = y \/ rotate_right x = rotate_right y) /\
  (!x y. ~x = y \/ second x = second y) /\
  (!x y. ~x = y \/ sigma x = sigma y) /\
  (!x y. ~x = y \/ singleton_set x = singleton_set y) /\
  (!x y. ~x = y \/ successor x = successor y) /\
  (!x y z. ~x = y \/ union x z = union y z) /\
  (!x y z. ~x = y \/ union z x = union z y) /\
  (!x y z. ~x = y \/ ~closed x z \/ closed y z) /\
  (!x y z. ~x = y \/ ~closed z x \/ closed z y) /\
  (!x y z. ~x = y \/ ~disjoint x z \/ disjoint y z) /\
  (!x y z. ~x = y \/ ~disjoint z x \/ disjoint z y) /\
  (!x y. ~x = y \/ ~function x \/ function y) /\
  (!x y z x' y' z'.
     ~x = y \/ ~homomorphism x z x' y' z' \/ homomorphism y z x' y' z') /\
  (!x y z x' y' z'.
     ~x = y \/ ~homomorphism z x x' y' z' \/ homomorphism z y x' y' z') /\
  (!x y z x' y' z'.
     ~x = y \/ ~homomorphism z x' x y' z' \/ homomorphism z x' y y' z') /\
  (!x y z x' y' z'.
     ~x = y \/ ~homomorphism z x' y' x z' \/ homomorphism z x' y' y z') /\
  (!x y z x' y' z'.
     ~x = y \/ ~homomorphism z x' y' z' x \/ homomorphism z x' y' z' y) /\
  (!x y. ~x = y \/ ~little_set x \/ little_set y) /\
  (!x y z v. ~x = y \/ ~maps x z v \/ maps y z v) /\
  (!x y z v. ~x = y \/ ~maps z x v \/ maps z y v) /\
  (!x y z v. ~x = y \/ ~maps z v x \/ maps z v y) /\
  (!x y z. ~x = y \/ ~member x z \/ member y z) /\
  (!x y z. ~x = y \/ ~member z x \/ member z y) /\
  (!x y. ~x = y \/ ~one_to_one_function x \/ one_to_one_function y) /\
  (!x y. ~x = y \/ ~ordered_pair_predicate x \/ ordered_pair_predicate y) /\
  (!x y z. ~x = y \/ ~proper_subset x z \/ proper_subset y z) /\
  (!x y z. ~x = y \/ ~proper_subset z x \/ proper_subset z y) /\
  (!x y. ~x = y \/ ~relation x \/ relation y) /\
  (!x y. ~x = y \/ ~single_valued_set x \/ single_valued_set y) /\
  (!x y z. ~x = y \/ ~subset x z \/ subset y z) /\
  (!x y z. ~x = y \/ ~subset z x \/ subset z y) /\
  (!x y z v w.
     ~associative x y \/ ~member z x \/ ~member v x \/ ~member w x \/
     apply_to_two_arguments y (apply_to_two_arguments y z v) w =
     apply_to_two_arguments y z (apply_to_two_arguments y v w)) /\
  (!x y. associative x y \/ member (f34 x y) x) /\
  (!x y. associative x y \/ member (f35 x y) x) /\
  (!x y. associative x y \/ member (f36 x y) x) /\
  (!x y.
     associative x y \/
     ~apply_to_two_arguments y (apply_to_two_arguments y (f34 x y) (f35 x y))
      (f36 x y) = apply_to_two_arguments y (f34 x y)
      (apply_to_two_arguments y (f35 x y) (f36 x y))) /\
  (!x y z. ~identity x y z \/ member z x) /\
  (!x y z v.
     ~identity x y z \/ ~member v x \/ apply_to_two_arguments y z v = v) /\
  (!x y z v.
     ~identity x y z \/ ~member v x \/ apply_to_two_arguments y v z = v) /\
  (!x y z. identity x y z \/ ~member z x \/ member (f37 x y z) x) /\
  (!x y z.
     identity x y z \/ ~member z x \/
     ~apply_to_two_arguments y z (f37 x y z) = f37 x y z \/
     ~apply_to_two_arguments y (f37 x y z) z = f37 x y z) /\
  (!x y z v. ~inverse x y z v \/ maps v x x) /\
  (!x y z v w.
     ~inverse x y z v \/ ~member w x \/
     apply_to_two_arguments y (apply v w) w = z) /\
  (!x y z v w.
     ~inverse x y z v \/ ~member w x \/
     apply_to_two_arguments y w (apply v w) = z) /\
  (!x y z v. inverse x y z v \/ ~maps v x x \/ member (f38 x y z v) x) /\
  (!x y z v.
     inverse x y z v \/ ~maps v x x \/
     ~apply_to_two_arguments y (apply v (f38 x y z v)) (f38 x y z v) = z \/
     ~apply_to_two_arguments y (f38 x y z v) (apply v (f38 x y z v)) = z) /\
  (!x y. ~group x y \/ closed x y) /\
  (!x y. ~group x y \/ associative x y) /\
  (!x y. ~group x y \/ identity x y (f39 x y)) /\
  (!x y. ~group x y \/ inverse x y (f39 x y) (f40 x y)) /\
  (!x y z v.
     group x y \/ ~closed x y \/ ~associative x y \/ ~identity x y z \/
     ~inverse x y z v) /\
  (!x y z v.
     ~commutes x y \/ ~member z x \/ ~member v x \/
     apply_to_two_arguments y z v = apply_to_two_arguments y v z) /\
  (!x y. commutes x y \/ member (f41 x y) x) /\
  (!x y. commutes x y \/ member (f42 x y) x) /\
  (!x y.
     commutes x y \/
     ~apply_to_two_arguments y (f41 x y) (f42 x y) = apply_to_two_arguments y
      (f42 x y) (f41 x y)) /\ (!x y z. ~x = y \/ f34 x z = f34 y z) /\
  (!x y z. ~x = y \/ f34 z x = f34 z y) /\
  (!x y z. ~x = y \/ f35 x z = f35 y z) /\
  (!x y z. ~x = y \/ f35 z x = f35 z y) /\
  (!x y z. ~x = y \/ f36 x z = f36 y z) /\
  (!x y z. ~x = y \/ f36 z x = f36 z y) /\
  (!x y z v. ~x = y \/ f37 x z v = f37 y z v) /\
  (!x y z v. ~x = y \/ f37 z x v = f37 z y v) /\
  (!x y z v. ~x = y \/ f37 z v x = f37 z v y) /\
  (!x y z v w. ~x = y \/ f38 x z v w = f38 y z v w) /\
  (!x y z v w. ~x = y \/ f38 z x v w = f38 z y v w) /\
  (!x y z v w. ~x = y \/ f38 z v x w = f38 z v y w) /\
  (!x y z v w. ~x = y \/ f38 z v w x = f38 z v w y) /\
  (!x y z. ~x = y \/ f39 x z = f39 y z) /\
  (!x y z. ~x = y \/ f39 z x = f39 z y) /\
  (!x y z. ~x = y \/ f40 x z = f40 y z) /\
  (!x y z. ~x = y \/ f40 z x = f40 z y) /\
  (!x y z. ~x = y \/ f41 x z = f41 y z) /\
  (!x y z. ~x = y \/ f41 z x = f41 z y) /\
  (!x y z. ~x = y \/ f42 x z = f42 y z) /\
  (!x y z. ~x = y \/ f42 z x = f42 z y) /\
  (!x y z. ~x = y \/ ~associative x z \/ associative y z) /\
  (!x y z. ~x = y \/ ~associative z x \/ associative z y) /\
  (!x y z. ~x = y \/ ~commutes x z \/ commutes y z) /\
  (!x y z. ~x = y \/ ~commutes z x \/ commutes z y) /\
  (!x y z. ~x = y \/ ~group x z \/ group y z) /\
  (!x y z. ~x = y \/ ~group z x \/ group z y) /\
  (!x y z v. ~x = y \/ ~identity x z v \/ identity y z v) /\
  (!x y z v. ~x = y \/ ~identity z x v \/ identity z y v) /\
  (!x y z v. ~x = y \/ ~identity z v x \/ identity z v y) /\
  (!x y z v w. ~x = y \/ ~inverse x z v w \/ inverse y z v w) /\
  (!x y z v w. ~x = y \/ ~inverse z x v w \/ inverse z y v w) /\
  (!x y z v w. ~x = y \/ ~inverse z v x w \/ inverse z v y w) /\
  (!x y z v w. ~x = y \/ ~inverse z v w x \/ inverse z v w y) /\
  (!x y.
     ~member x natural_numbers \/ ~little_set y \/ ~member empty_set y \/
     member (f43 x y) y \/ member x y) /\
  (!x y.
     ~member x natural_numbers \/ ~little_set y \/ ~member empty_set y \/
     ~member (successor (f43 x y)) y \/ member x y) /\
  (!x. member x natural_numbers \/ ~little_set x \/ little_set (f44 x)) /\
  (!x.
     member x natural_numbers \/ ~little_set x \/
     member empty_set (f44 x)) /\
  (!x y.
     member x natural_numbers \/ ~little_set x \/ ~member y (f44 x) \/
     member (successor y) (f44 x)) /\
  (!x. member x natural_numbers \/ ~member x (f44 x)) /\
  (!x y.
     ~member x plus \/ ~little_set y \/ member (f45 x y) natural_numbers \/
     member (f46 x y) natural_numbers \/ member x y) /\
  (!x y.
     ~member x plus \/ ~little_set y \/ member (f45 x y) natural_numbers \/
     member (f47 x y) natural_numbers \/ member x y) /\
  (!x y.
     ~member x plus \/ ~little_set y \/ member (f45 x y) natural_numbers \/
     member (f48 x y) natural_numbers \/ member x y) /\
  (!x y.
     ~member x plus \/ ~little_set y \/ member (f45 x y) natural_numbers \/
     member (ordered_pair (ordered_pair (f46 x y) (f47 x y)) (f48 x y)) y \/
     member x y) /\
  (!x y.
     ~member x plus \/ ~little_set y \/ member (f45 x y) natural_numbers \/
     ~member
      (ordered_pair (ordered_pair (successor (f46 x y)) (f47 x y))
       (successor (f48 x y))) y \/ member x y) /\
  (!x y.
     ~member x plus \/ ~little_set y \/
     ~member (ordered_pair (ordered_pair empty_set (f45 x y)) (f45 x y)) y \/
     member (f46 x y) natural_numbers \/ member x y) /\
  (!x y.
     ~member x plus \/ ~little_set y \/
     ~member (ordered_pair (ordered_pair empty_set (f45 x y)) (f45 x y)) y \/
     member (f47 x y) natural_numbers \/ member x y) /\
  (!x y.
     ~member x plus \/ ~little_set y \/
     ~member (ordered_pair (ordered_pair empty_set (f45 x y)) (f45 x y)) y \/
     member (f48 x y) natural_numbers \/ member x y) /\
  (!x y.
     ~member x plus \/ ~little_set y \/
     ~member (ordered_pair (ordered_pair empty_set (f45 x y)) (f45 x y)) y \/
     member (ordered_pair (ordered_pair (f46 x y) (f47 x y)) (f48 x y)) y \/
     member x y) /\
  (!x y.
     ~member x plus \/ ~little_set y \/
     ~member (ordered_pair (ordered_pair empty_set (f45 x y)) (f45 x y)) y \/
     ~member
      (ordered_pair (ordered_pair (successor (f46 x y)) (f47 x y))
       (successor (f48 x y))) y \/ member x y) /\
  (!x. member x plus \/ ~little_set x \/ little_set (f49 x)) /\
  (!x y.
     member x plus \/ ~little_set x \/ ~member y natural_numbers \/
     member (ordered_pair (ordered_pair empty_set y) y) (f49 x)) /\
  (!x y z v.
     member x plus \/ ~little_set x \/ ~member y natural_numbers \/
     ~member z natural_numbers \/ ~member v natural_numbers \/
     ~member (ordered_pair (ordered_pair y z) v) (f49 x) \/
     member (ordered_pair (ordered_pair (successor y) z) (successor v))
     (f49 x)) /\ (!x. member x plus \/ ~member x (f49 x)) /\
  (!x y.
     ~member x times \/ ~little_set y \/ member (f50 x y) natural_numbers \/
     member (f51 x y) natural_numbers \/ member x y) /\
  (!x y.
     ~member x times \/ ~little_set y \/ member (f50 x y) natural_numbers \/
     member (f52 x y) natural_numbers \/ member x y) /\
  (!x y.
     ~member x times \/ ~little_set y \/ member (f50 x y) natural_numbers \/
     member (f53 x y) natural_numbers \/ member x y) /\
  (!x y.
     ~member x times \/ ~little_set y \/ member (f50 x y) natural_numbers \/
     member (ordered_pair (ordered_pair (f51 x y) (f52 x y)) (f53 x y)) y \/
     member x y) /\
  (!x y.
     ~member x times \/ ~little_set y \/ member (f50 x y) natural_numbers \/
     ~member
      (ordered_pair (ordered_pair (successor (f51 x y)) (f52 x y))
       (apply_to_two_arguments plus (f53 x y) (f52 x y))) y \/ member x y) /\
  (!x y.
     ~member x times \/ ~little_set y \/
     ~member (ordered_pair (ordered_pair empty_set (f50 x y)) empty_set) y \/
     member (f51 x y) natural_numbers \/ member x y) /\
  (!x y.
     ~member x times \/ ~little_set y \/
     ~member (ordered_pair (ordered_pair empty_set (f50 x y)) empty_set) y \/
     member (f52 x y) natural_numbers \/ member x y) /\
  (!x y.
     ~member x times \/ ~little_set y \/
     ~member (ordered_pair (ordered_pair empty_set (f50 x y)) empty_set) y \/
     member (f53 x y) natural_numbers \/ member x y) /\
  (!x y.
     ~member x times \/ ~little_set y \/
     ~member (ordered_pair (ordered_pair empty_set (f50 x y)) empty_set) y \/
     member (ordered_pair (ordered_pair (f51 x y) (f52 x y)) (f53 x y)) y \/
     member x y) /\
  (!x y.
     ~member x times \/ ~little_set y \/
     ~member (ordered_pair (ordered_pair empty_set (f50 x y)) empty_set) y \/
     ~member
      (ordered_pair (ordered_pair (successor (f51 x y)) (f52 x y))
       (apply_to_two_arguments plus (f53 x y) (f52 x y))) y \/ member x y) /\
  (!x. member x times \/ ~little_set x \/ little_set (f54 x)) /\
  (!x y.
     member x times \/ ~little_set x \/ ~member y natural_numbers \/
     member (ordered_pair (ordered_pair empty_set y) empty_set) (f54 x)) /\
  (!x y z v.
     member x times \/ ~little_set x \/ ~member y natural_numbers \/
     ~member z natural_numbers \/ ~member v natural_numbers \/
     ~member (ordered_pair (ordered_pair y z) v) (f54 x) \/
     member
     (ordered_pair (ordered_pair (successor y) z)
      (apply_to_two_arguments plus v z)) (f54 x)) /\
  (!x. member x times \/ ~member x (f54 x)) /\
  (!x. ~member x prime_numbers \/ member x natural_numbers) /\
  (!x. ~member x prime_numbers \/ ~x = empty_set) /\
  (!x. ~member x prime_numbers \/ ~x = successor empty_set) /\
  (!x y z.
     ~member x prime_numbers \/ ~member y natural_numbers \/
     ~member z natural_numbers \/ ~apply_to_two_arguments times y z = x \/
     member y (non_ordered_pair (successor empty_set) x)) /\
  (!x.
     member x prime_numbers \/ ~member x natural_numbers \/ x = empty_set \/
     x = successor empty_set \/ member (f55 x) natural_numbers) /\
  (!x.
     member x prime_numbers \/ ~member x natural_numbers \/ x = empty_set \/
     x = successor empty_set \/ member (f56 x) natural_numbers) /\
  (!x.
     member x prime_numbers \/ ~member x natural_numbers \/ x = empty_set \/
     x = successor empty_set \/
     apply_to_two_arguments times (f55 x) (f56 x) = x) /\
  (!x.
     member x prime_numbers \/ ~member x natural_numbers \/ x = empty_set \/
     x = successor empty_set \/
     ~member (f55 x) (non_ordered_pair (successor empty_set) x)) /\
  (!x. ~finite x \/ member (f57 x) natural_numbers) /\
  (!x. ~finite x \/ maps (f58 x) (f57 x) x) /\
  (!x. ~finite x \/ range_of (f58 x) = x) /\
  (!x. ~finite x \/ one_to_one_function (f58 x)) /\
  (!x y z.
     finite x \/ ~member y natural_numbers \/ ~maps z y x \/
     ~range_of z = x \/ ~one_to_one_function z) /\
  (!x. ~member x twin_prime_numbers \/ member x prime_numbers) /\
  (!x.
     ~member x twin_prime_numbers \/
     member (successor (successor x)) prime_numbers) /\
  (!x.
     member x twin_prime_numbers \/ ~member x prime_numbers \/
     ~member (successor (successor x)) prime_numbers) /\
  (!x. ~member x even_numbers \/ member x natural_numbers) /\
  (!x. ~member x even_numbers \/ member (f59 x) natural_numbers) /\
  (!x.
     ~member x even_numbers \/
     apply_to_two_arguments plus (f59 x) (f59 x) = x) /\
  (!x y.
     member x even_numbers \/ ~member x natural_numbers \/
     ~member y natural_numbers \/ ~apply_to_two_arguments plus y y = x) /\
  (!x y z. ~x = y \/ f43 x z = f43 y z) /\
  (!x y z. ~x = y \/ f43 z x = f43 z y) /\ (!x y. ~x = y \/ f44 x = f44 y) /\
  (!x y z. ~x = y \/ f45 x z = f45 y z) /\
  (!x y z. ~x = y \/ f45 z x = f45 z y) /\
  (!x y z. ~x = y \/ f46 x z = f46 y z) /\
  (!x y z. ~x = y \/ f46 z x = f46 z y) /\
  (!x y z. ~x = y \/ f47 x z = f47 y z) /\
  (!x y z. ~x = y \/ f47 z x = f47 z y) /\
  (!x y z. ~x = y \/ f48 x z = f48 y z) /\
  (!x y z. ~x = y \/ f48 z x = f48 z y) /\ (!x y. ~x = y \/ f49 x = f49 y) /\
  (!x y z. ~x = y \/ f50 x z = f50 y z) /\
  (!x y z. ~x = y \/ f50 z x = f50 z y) /\
  (!x y z. ~x = y \/ f51 x z = f51 y z) /\
  (!x y z. ~x = y \/ f51 z x = f51 z y) /\
  (!x y z. ~x = y \/ f52 x z = f52 y z) /\
  (!x y z. ~x = y \/ f52 z x = f52 z y) /\
  (!x y z. ~x = y \/ f53 x z = f53 y z) /\
  (!x y z. ~x = y \/ f53 z x = f53 z y) /\ (!x y. ~x = y \/ f54 x = f54 y) /\
  (!x y. ~x = y \/ f55 x = f55 y) /\ (!x y. ~x = y \/ f56 x = f56 y) /\
  (!x y. ~x = y \/ f57 x = f57 y) /\ (!x y. ~x = y \/ f58 x = f58 y) /\
  (!x y. ~x = y \/ f59 x = f59 y) /\
  (!x y. ~x = y \/ ~finite x \/ finite y) ==>
  ~member empty_set natural_numbers ==> F`;
