(load "./common/overrides.scm")
(load "./common/utils.scm")
(load "./common/collections.scm")
(load "./common/predicates.scm")
(load "./common/generic-procedures.scm")
(load "./common/applicability.scm")
(load "./common/match-utils.scm")
(load "./common/predicate-counter.scm")
;n어쩌구로 시작하는건 overrides.scm을 추가해야한다
(load "./pattern-matching-on-graphs/graph.scm")
(load "./pattern-matching-on-graphs/chess-board.scm")
(load "./pattern-matching-on-graphs/chess-moves.scm")

(define all-knight-moves
    (symmetrize-move basic-knight-move
        reflect-ew rotate-90 rotate-180))
