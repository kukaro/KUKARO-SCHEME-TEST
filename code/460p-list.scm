(define a-list (list 6 946 8 356 12 620))
(display a-list)
(newline)
(display (list? a-list))
(newline)
;리스트 인가
(display (car a-list))
(newline)
;맨 앞의 원소를 뺀다
(display (cdr a-list))
(newline)
;맨 앞의 원소를 뺀 나머지 원소를 list로 출력한다
(display (list-ref a-list 1))
(newline)
;두번째 원소를 출력한다(0번이 첫번째)