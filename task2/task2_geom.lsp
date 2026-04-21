#!/usr/bin/sbcl --script
;chmod +x ./geom.lsp

; Библиотека для работы с дробями
;------\
; Создаёт из двух целых чисел дробь вида (a b), где a - числитель, b - знаменатель
(defun create_frac (a b) (list a b))

; Создаёт из целого числа дробь вида (a b), где a - числитель, b - знаменатель
(defun init_frac (a) (create_frac a 1))

; Возвращает числитель дроби из списка
(defun num (f) (car f))

; Возвращает знаменатель дроби из списка
(defun den (f) (cadr f))

; Поиск НОД числителя и знаменателя дроби алгоритмом Евклида
(defun get_gcd (f) (gget_gcd (num f) (den f)))
; Вспомогательная функция для get_gcd. Проверяет знак числителя и знаменателя
(defun gget_gcd (a b)
    (cond ((< a 0) (cond ((< b 0) (ggget_gcd (- a) (- b)))
                         (T (ggget_gcd (- a) b))
                   ))
          ((< b 0) (ggget_gcd a (- b)))
          (T (ggget_gcd a b))
    )
)
; Вспомогательная функция для gget_gcd. Реализация алгоритма Евклида
(defun ggget_gcd (a b)
    (cond ((eql a 0) b)
          ((eql b 0) a)
          ((eql a b) a)
          ((< a b) (gget_gcd a (- b a)))
          (T (gget_gcd (- a b) b))
    )
)

; Сокращает дробь
(defun reduce_frac (f) 
    (cond ((< (den f) 0) (reduce_frac (create_frac (- (num f)) (- (den f)))))
          (T (red_frac f (get_gcd f)))
    )
)
; Вспомогательная функция с накапливающим параметром для reduce_frac
(defun red_frac (f gd) (create_frac (/ (num f) gd) (/ (den f) gd)))

; Создание дроби, противоположной по знаку
(defun neg_frac (f) (create_frac (- (num f)) (den f)))

; "Переворачивание" дроби. Возвращает дробь вида (b a) из дроби вида (a b)
(defun flip_frac (f) (create_frac (den f) (num f)))

; Суммирование дробей
(defun sum_frac (f1 f2) (reduce_frac (create_frac (+ (* (num f1) (den f2)) (* (num f2) (den f1))) (* (den f1) (den f2)))))

; Вычитание дробей
(defun sub_frac (f1 f2) (sum_frac f1 (neg_frac f2)))

; Умножение дробей
(defun mul_frac (f1 f2) (reduce_frac (create_frac (* (num f1) (num f2)) (* (den f1) (den f2)))))

; Деление дробей
(defun div_frac (f1 f2) (mul_frac f1 (flip_frac f2)))

; Проверка равенства дробей
(defun eq_frac (f1 f2) (eqq_frac (reduce_frac f1) (reduce_frac f2)))
; Вспомогательная функция для eq_frac
(defun eqq_frac (f1 f2) (cond ((eql (num f1) (num f2)) (eql (den f1) (den f2)))))

; Проверка, что дробь равна нулю
(defun zero_frac (f) (eql 0 (num (reduce_frac f))))

; Сравнение дробей
(defun cmp_frac (f1 f2) (cmpp_frac (reduce_frac f1) (reduce_frac f2)))
; Вспомогательная функция для cmp_frac. Возвращает -1 если f1 < f2, 0 если равны, 1 если f1 > f2
(defun cmpp_frac (f1 f2)
    (cond ((eql (* (num f1) (den f2)) (* (num f2) (den f1))) 0)
          ((<   (* (num f1) (den f2)) (* (num f2) (den f1))) -1)
          (T 1)
    )
)

; Знак дроби: -1 / 0 / 1
(defun sign_frac (f) (ssign_frac (reduce_frac f)))
; Вспомогательная функция для sign_frac
(defun ssign_frac (f)
    (cond ((eql (num f) 0) 0)
          ((< (num f) 0) -1)
          (T 1)
    )
)

; Максимум из двух дробей
(defun max_frac (f1 f2)
    (cond ((> (cmp_frac f1 f2) 0) f1)
          (T f2)
    )
)

; Минимум из двух дробей
(defun min_frac (f1 f2)
    (cond ((< (cmp_frac f1 f2) 0) f1)
          (T f2)
    )
)

; Выводит дроби в виде рационального числа num/den (через /)
(defun print_frac (f) (princ (/ (num (reduce_frac f)) (den (reduce_frac f)))))

; Квадрат дроби
(defun sqr_frac (f) (mul_frac f f))

; Абсолютное значение дроби
(defun abs_frac (f)
    (cond ((< (sign_frac f) 0) (neg_frac f))
          (T f)
    )
)
;------/

; Библиотека для работы с точками
;------\
; Приведение целого числа a к виду дроби (a 1)
(defun normalize_point (p) (list (init_frac (car p)) (init_frac (cadr p))))
; Приведение списка целых чисел к виду дроби
(defun normalize_points (p_list) (mapcar #'normalize_point p_list))

; Получение x координаты точки из списка
(defun get_x (p) (car p))

; Получение y координаты точки из списка
(defun get_y (p) (cadr p))

; Проверяет, что две точки совпадают
(defun eq_point (p1 p2) (cond ((eq_frac (get_x p1) (get_x p2)) (eq_frac (get_y p1) (get_y p2)))))

; Проверяет наличие точки в списке точек
(defun member_point (p_list p)
    (cond ((null p_list) nil)
          ((eq_point (car p_list) p) T)
          (T (member_point (cdr p_list) p))
    )
)

; Добавляет точку в список, избегая повторов
(defun add_point (p_list p)
    (cond ((member_point p_list p) p_list)
          (T (cons p p_list))
    )
)

; Соединяет два списка точек в единый
(defun merge_points (p_list1 p_list2)
    (cond ((null p_list2) p_list1)
          (T (merge_points (add_point p_list1 (car p_list2)) (cdr p_list2)))
    )
)

; Выводит точку в формате (x y)
(defun print_point (p)
    (princ "  (")
    (princ (/ (num (get_x p)) (den (get_x p))))
    (princ " ")
    (princ (/ (num (get_y p)) (den (get_y p))))
    (princ ") ")
)

; Выводит список точек
(defun print_points (p_list)
    (cond ((null p_list) nil)
          (T (print_point (car p_list)) (print_points (cdr p_list)))
    )
)
;------/

; Библиотека для работы с прямыми
;------\
; Вычисление коэффициента А по формуле A = (y2 − y1)
(defun calc_A (p1 p2) (sub_frac (get_y p2) (get_y p1)))

; Вычисление коэффициента B по формуле B = (x1 − x2)
(defun calc_B (p1 p2) (sub_frac (get_x p1) (get_x p2)))

; Вычисление коэффициента C по формуле C = (x2*y1 − x1*y2)
(defun calc_C (p1 p2) (sub_frac (mul_frac (get_x p2) (get_y p1)) (mul_frac (get_x p1) (get_y p2))))

; Получение коэффициента A прямой из списка
(defun get_A (l) (caar l))

; Получение коэффициента B прямой из списка
(defun get_B (l) (cadar l))

; Получение коэффициента C прямой из списка
(defun get_C (l) (caddar l))

; Получает список точек, лежащих на прямой
(defun get_points (l) (cadr l))

; Создаёт прямую в формате ((A B C) ((x y))
(defun create_line (p1 p2) (list (list (calc_A p1 p2) (calc_B p1 p2) (calc_C p1 p2)) (list p1 p2)))

; Проверка параллельности прямых по формуле A1*B2 - A2*B1 = 0
(defun parallel_line (l1 l2) (zero_frac (sub_frac (mul_frac (get_A l1) (get_B l2)) (mul_frac (get_A l2) (get_B l1)))))

; Проверка перпендикулярности прямых по формуле A1*A2 + B1*B2 = 0
(defun perpendicular_line (l1 l2) (zero_frac (sum_frac (mul_frac (get_A l1) (get_A l2)) (mul_frac (get_B l1) (get_B l2)))))

; Проверяет на равенство две прямые через пропорциональность коэффициентов
(defun eq_line (l1 l2)
    (and (zero_frac (sub_frac (mul_frac (get_A l1) (get_B l2)) (mul_frac (get_A l2) (get_B l1))))
         (zero_frac (sub_frac (mul_frac (get_A l1) (get_C l2)) (mul_frac (get_A l2) (get_C l1))))
         (zero_frac (sub_frac (mul_frac (get_B l1) (get_C l2)) (mul_frac (get_B l2) (get_C l1))))
    )
)

; Обновляет список точек прямой (добавляет новые)
(defun merge_lines (l p_list) (list (car l) (merge_points (get_points l) p_list)))

; Вставляет линию в список линий. Если линия уже есть, объединяет списки точек, иначе добавляет новую
(defun add_line (cur_l l_list)
    (cond ((null l_list) (cons cur_l nil))
          ((eq_line cur_l (car l_list)) (cons (merge_lines (car l_list) (get_points cur_l)) (cdr l_list)))
          (T (cons (car l_list) (add_line cur_l (cdr l_list))))
    )
)

; Поиск всех прямых по набору точек
(defun get_lines (p_list l_list)
    (cond ((null p_list) l_list)
          ((null (cdr p_list)) l_list)
          (T (get_lines (cdr p_list) (gget_lines (car p_list) (cdr p_list) l_list)))
    )
)
; Вспомогательная функция для get_lines
(defun gget_lines (cur_p rest_p l_list)
    (cond ((null rest_p) l_list)
          ((eq_point cur_p (car rest_p)) (gget_lines cur_p (cdr rest_p) l_list))
          (T (gget_lines cur_p (cdr rest_p) (add_line (create_line cur_p (car rest_p)) l_list)))
    )
)

; Считает определитель по формуле det = A1*B2 - A2*B1. Если det = 0, прямые параллельны или совпадают (нет единственной точки пересечения)
(defun det_lines (l1 l2) (sub_frac (mul_frac (get_A l1) (get_B l2)) (mul_frac (get_A l2) (get_B l1))))

; Считает координаты точек пересечения методом Крамера (x = (B1*C2 - B2*C1) / det; y = (C1*A2 - C2*A1) / det)
(defun intersect_lines (l1 l2) (int_lines l1 l2 (det_lines l1 l2)))
; Вспомогательная функция для intersect_lines
(defun int_lines (l1 l2 det) 
    (list (div_frac (sub_frac (mul_frac (get_B l1) (get_C l2)) (mul_frac (get_B l2) (get_C l1))) det)
          (div_frac (sub_frac (mul_frac (get_C l1) (get_A l2)) (mul_frac (get_C l2) (get_A l1))) det)
    )
)

; Выводит прямую в формате Ax + By + C = 0
(defun print_line (l) 
    (princ "Прямая ") 
    (princ (/ (num (get_A l)) (den (get_A l))))
    (princ "x + ")
    (princ (/ (num (get_B l)) (den (get_B l))))
    (princ "y + ")
    (princ (/ (num (get_C l)) (den (get_C l))))
    (princ " = 0")
    (terpri)
)

; Выводит список прямых
(defun print_lines (l_list)
    (cond ((null l_list) nil)
          (T (print_line (car l_list))
             (princ "Точки на прямой:")
             (print_points (get_points (car l_list)))
             (terpri)
             (print_lines (cdr l_list)))
    )
)

; Выводит пары прямых
(defun print_line_pairs (l_pairs)
    (cond ((null l_pairs) nil)
          (T (print_line (caar l_pairs)) (print_line (cadar l_pairs)) (terpri) (print_line_pairs (cdr l_pairs)))
    )
)

; Функция активации пункта a
(defun geom_a (p_list) (activate_geom_a (normalize_points p_list) (get_lines (normalize_points p_list) nil)))
; Вспомогательная функция с накапливающим параметром для geom_a
(defun activate_geom_a (p_list l_list)
    (cond ((null p_list) (princ "ОШИБКА: Недостаточно точек для построения прямой") (terpri))
          ((null (cdr p_list)) (princ "ОШИБКА: Недостаточно точек для построения прямой") (terpri))
          ((null l_list) (princ "ОШИБКА: Невозможно построить прямые (слишком мало различных точек)"))
          (T (princ "Исходные точки:") (terpri)
             (print_points p_list) (terpri)
             (princ "==============================") (terpri)
             (princ "ПУНКТ а") (terpri)
             (princ "Все прямые, порождаемые точками:") (terpri) (terpri)
             (print_lines l_list)
             (princ "==============================") (terpri)
             (princ "ПОДПУНКТ a1 Параллельные прямые (пары):") (terpri) (terpri)
             (print_line_pairs (a1_parallel l_list))
             (princ "==============================") (terpri)
             (princ "ПОДПУНКТ a1 Перпендикулярные прямые (пары):") (terpri) (terpri)
             (print_line_pairs (a1_perpendicular l_list))
             (princ "==============================") (terpri)
             (princ "ПОДПУНКТ a2 Прямые, содержащие более двух исходных точек:") (terpri) (terpri)
             (print_lines (a2 l_list))
             (princ "==============================") (terpri)
             (princ "ПОДПУНКТ a3 Все точки пересечения получившихся прямых (уникальные):") (terpri) (terpri)
             (print_points (a3 l_list)) (terpri)
             (princ "==============================") (terpri))
    )
)

; Функция активации подпункта a1 - параллельные прямые
(defun a1_parallel (l_list)
    (cond ((null l_list) nil)
          (T (activate_a1_parallel l_list (cdr l_list) (car l_list) nil))
    )
)
; Вспомогательная функция с накапливающим параметром для a1_parallel
(defun activate_a1_parallel (start rest_l cur_l paral_l)
    (cond ((null start) paral_l)
          ((null rest_l) (activate_a1_parallel (cdr start) (cddr start) (cadr start) paral_l))
          ((parallel_line cur_l (car rest_l)) (activate_a1_parallel start (cdr rest_l) cur_l (cons (list cur_l (car rest_l)) paral_l)))
          (T (activate_a1_parallel start (cdr rest_l) cur_l paral_l))
    )
)

; Функция активации подпункта a1 - перпендикулярные прямые
(defun a1_perpendicular (l_list)
    (cond ((null l_list) nil)
          (T (activate_a1_perpendicular l_list (cdr l_list) (car l_list) nil))
    )
)
; Вспомогательная функция с накапливающим параметром для a1_perpendicular
(defun activate_a1_perpendicular (start rest_l cur_l perpen_l)
    (cond ((null start) perpen_l)
          ((null rest_l) (activate_a1_perpendicular (cdr start) (cddr start) (cadr start) perpen_l))
          ((perpendicular_line cur_l (car rest_l)) (activate_a1_perpendicular start (cdr rest_l) cur_l (cons (list cur_l (car rest_l)) perpen_l)))
          (T (activate_a1_perpendicular start (cdr rest_l) cur_l perpen_l))
    )
)

; Функция активации подпункта a2
(defun a2 (l_list)
    (cond ((null l_list) nil)
          ((has_over_2_points (car l_list)) (cons (car l_list) (a2 (cdr l_list))))
          (T (a2 (cdr l_list)))
    )
)
; Проверка прямой на наличие более 2 точек
(defun has_over_2_points (l)
    (cond ((null (cddr (get_points l))) nil)
          (T T)
    )
)

; Функция активации подпункта a3
(defun a3 (l_list)
    (cond ((null l_list) nil)
          (T (activate_a3 l_list (cdr l_list) (car l_list) nil))
    )
)
; Вспомогательная функция с накапливающим параметром для a3
(defun activate_a3 (start rest_l cur_l inter_p)
    (cond ((null start) inter_p)
          ((null rest_l) (activate_a3 (cdr start) (cddr start) (cadr start) inter_p))
          ((zero_frac (det_lines cur_l (car rest_l))) (activate_a3 start (cdr rest_l) cur_l inter_p))
          (T (activate_a3 start (cdr rest_l) cur_l (add_point inter_p (intersect_lines cur_l (car rest_l)))))
    )
)
;------/

;------\
;; Тесты для пункта а

;; (geom_a '())
;; (geom_a '((0 0)))
;; (geom_a '((0 0) (1 1)))
;; (geom_a '((0 0) (2 0) (0 1)))
;; (geom_a '((0 0) (1 1) (2 2)))
;; (geom_a '((0 0) (1 1) (1 1) (2 2)))
;; (geom_a '((0 0) (1 0) (2 0) (1 1)))
;; (geom_a '((0 0) (2 0) (2 1) (0 1)))
;; (geom_a '((0 0) (1 1) (2 2) (0 2) (2 0)))
;; (geom_a '((-6 4) (-5 -1) (1 7) (2 2)))
;------/

; Библиотека для работы с особыми точками
;------\
; Выводит пары точек
(defun print_point_pair (pair)
    (princ "  ")
    (print_point (car pair))
    (princ "  ")
    (print_point (cadr pair))
    (terpri)
)

; Выводит списка пар точек
(defun print_point_pairs (pairs)
    (cond ((null pairs) nil)
          (T (print_point_pair (car pairs)) (print_point_pairs (cdr pairs)))
    )
)

; Квадрат расстояния между двумя точками пары
(defun pair_dist2 (pair) (dist2 (car pair) (cadr pair)))
; Квадрат расстояния между двумя точками: ((x1-x2)^2 + (y1-y2)^2)
(defun dist2 (p1 p2)
    (sum_frac (mul_frac (sub_frac (get_x p1) (get_x p2)) (sub_frac (get_x p1) (get_x p2)))
              (mul_frac (sub_frac (get_y p1) (get_y p2)) (sub_frac (get_y p1) (get_y p2))))
)

; Выбирает пару с меньшим dist2
(defun min_dist (pair1 pair2)
    (cond ((< (cmp_frac (pair_dist2 pair1) (pair_dist2 pair2)) 0) pair1)
          (T pair2)
    )
)

; Выбирает пару с большим dist2
(defun max_dist (pair1 pair2)
    (cond ((> (cmp_frac (pair_dist2 pair1) (pair_dist2 pair2)) 0) pair1)
          (T pair2)
    )
)

; Уникализация набора точек
(defun unique_points (p_list) (uniq_points p_list nil))
; Вспомогательная функция с накапливающим параметром для unique_points
(defun uniq_points (p_list unique_list)
    (cond ((null p_list) unique_list)
          (T (uniq_points (cdr p_list) (add_point unique_list (car p_list))))
    )
)

; Минимальная x-координата точки
(defun min_x (p_list)
    (cond ((null (cdr p_list)) (get_x (car p_list)))
          (T (min_frac (get_x (car p_list)) (min_x (cdr p_list))))
    )
)

; Максимальная x-координата точки
(defun max_x (p_list)
    (cond ((null (cdr p_list)) (get_x (car p_list)))
          (T (max_frac (get_x (car p_list)) (max_x (cdr p_list))))
    )
)

; Минимальная y-координата точки
(defun min_y (p_list)
    (cond ((null (cdr p_list)) (get_y (car p_list)))
          (T (min_frac (get_y (car p_list)) (min_y (cdr p_list))))
    )
)

; Максимальная y-координата точки
(defun max_y (p_list)
    (cond ((null (cdr p_list)) (get_y (car p_list)))
          (T (max_frac (get_y (car p_list)) (max_y (cdr p_list))))
    )
)

; Выводит окружность
(defun print_circle (circ)
    (cond ((null circ) (princ "ОШИБКА: окружность не построена") (terpri))
          (T (princ "Окружность: центр = ")
             (print_point (car circ))
             (princ "  радиус = ")
             (print_frac (cadr circ))
             (terpri))
    )
)

; Выводит пары точек, образующих разделяющую прямую
(defun print_classification_result (pairs)
    (cond ((null pairs) nil)
          (T (print_lines (get_class_res pairs nil)))
    )
)

(defun get_class_res (pairs l_list)
    (cond ((null pairs) l_list)
          (T (get_class_res (cdr pairs) (add_line (car (get_lines (car pairs) nil)) l_list)))
    )
)

; Функция активации пункта d
(defun geom_d (p_list) (activate_geom_d (unique_points (normalize_points p_list))))
; Вспомогательная функция с накапливающим параметром для geom_d
(defun activate_geom_d (p_list)
    (princ "Исходные точки:") (terpri)
    (print_points p_list) (terpri)
    (princ "==============================") (terpri)
    (princ "ПУНКТ d") (terpri)
    (princ "==============================") (terpri)
    (princ "ПОДПУНКТ d1: ближайшая и самая дальняя пары") (terpri)
    (cond ((null p_list) (princ "ОШИБКА: недостаточно точек для поиска пар") (terpri))
          ((null (cdr p_list)) (princ "ОШИБКА: недостаточно точек для поиска пар") (terpri))
          (T (princ "Ближайшая пара:") (terpri)
             (print_point_pair (d1_near p_list))
             (princ "Самая дальняя пара:") (terpri)
             (print_point_pair (d1_further p_list)))
    )
    (princ "==============================") (terpri)
    (princ "ПОДПУНКТ d2: окружность, содержащая все точки") (terpri)
    (print_circle (d2 p_list))
    (princ "==============================") (terpri)
    (princ "ПОДПУНКТ d3: пары точек, делящие остальные поровну по сторонам") (terpri)
    (cond ((null p_list) (princ "ОШИБКА: недостаточно точек для классификации") (terpri))
          ((null (cdr p_list)) (princ "ОШИБКА: недостаточно точек для классификации") (terpri))
          (T (print_classification_result (d3 p_list)))
    )
    (princ "==============================") (terpri)
)

; Функция активации подпункта d1 - ближайшая пара точек
(defun d1_near (p_list)
    (cond ((null (cdr p_list)) nil)
          ((null (cddr p_list)) (list (car p_list) (cadr p_list)))
          (T (min_dist (d1_nr (car p_list) (cdr p_list)) (d1_near (cdr p_list))))
    )
)
; Вспомогательная функция для d1_near
(defun d1_nr (cur_p rest_p)
    (cond ((null (cdr rest_p)) (list cur_p (car rest_p)))
          (T (min_dist (list cur_p (car rest_p)) (d1_nr cur_p (cdr rest_p))))
    )
)

; Функция активации подпункта d1 - самая дальняя пара точек
(defun d1_further (p_list)
    (cond ((null (cdr p_list)) nil)
          ((null (cddr p_list)) (list (car p_list) (cadr p_list)))
          (T (max_dist (d1_fr (car p_list) (cdr p_list)) (d1_further (cdr p_list))))
    )
)
; Вспомогательная функция для d1_further
(defun d1_fr (cur_p rest_p)
    (cond ((null (cdr rest_p)) (list cur_p (car rest_p)))
          (T (max_dist (list cur_p (car rest_p)) (d1_fr cur_p (cdr rest_p))))
    )
)

; Функция активации подпункта d2 - особая окружность
(defun d2 (p_list)
    (cond ((null p_list) nil)
          (T (d2_activate (min_x p_list) (max_x p_list) (min_y p_list) (max_y p_list)))
    )
)
; Вспомогательная функция для d2
(defun d2_activate (mnx mxx mny mxy)
    (list
        (list
            (div_frac (sum_frac mxx mnx) (create_frac 2 1))
            (div_frac (sum_frac mxy mny) (create_frac 2 1))
        )
        (sum_frac (max_frac (sub_frac mxx mnx) (sub_frac mxy mny)) (init_frac 1))
    )
)

; Для данной прямой считает значение s = A*x + B*y + C: s>0 — одна сторона, s<0 — другая, s=0 — на прямой (не считаем).
(defun line_value (l p) (sum_frac (sum_frac (mul_frac (get_A l) (get_x p)) (mul_frac (get_B l) (get_y p))) (get_C l)))

; Проверка равенства множеств по обе стороны от линии
(defun split_eq (p1 p2 p_list) (split_eqq (create_line p1 p2) p1 p2 p_list 0 0))
; Вспомогательная функция с накапливающими параметрами для split_eq
(defun split_eqq (l p1 p2 p_list pos neg)
    (cond ((null p_list) (eql pos neg))
          ((or (eq_point (car p_list) p1) (eq_point (car p_list) p2)) (split_eqq l p1 p2 (cdr p_list) pos neg))
          (T (split_eqqq l p1 p2 (car p_list) (cdr p_list) pos neg))
    )
)
; Вспомогательная функция с накапливающими параметрами для split_eqq
(defun split_eqqq (l p1 p2 cur_p rest_p pos neg)
    (cond ((eql (sign_frac (line_value l cur_p)) 0) (split_eqq l p1 p2 rest_p pos neg))
          ((eql (sign_frac (line_value l cur_p)) 1) (split_eqq l p1 p2 rest_p (+ pos 1) neg))
          (T (split_eqq l p1 p2 rest_p pos (+ neg 1)))
    )
)

; Функция активации подпункта d3 - разделяющая прямая
(defun d3 (p_list)
    (cond ((null (cdr p_list)) nil)
          ((null (cddr p_list)) (list (list (car p_list) (cadr p_list))))
          (T (d3_activate p_list p_list (cdr p_list) nil))
    )
)
; Вспомогательная функция с накапливающими параметрами для d3
(defun d3_activate (all_l cur_l rest_l ans)
    (cond ((null (cdr cur_l)) ans)
          ((null rest_l) (d3_activate all_l (cdr cur_l) (cddr cur_l) ans))
          ((split_eq (car cur_l) (car rest_l) all_l) (d3_activate all_l cur_l (cdr rest_l) (cons (list (car cur_l) (car rest_l)) ans)))
          (T (d3_activate all_l cur_l (cdr rest_l) ans))
    )
)
;------/

; Тесты для пункта d
;------\
;; (geom_d '())
;; (geom_d '((0 0)))
;; (geom_d '((0 0) (0 0)))
;; (geom_d '((0 0) (5 0)))
;; (geom_d '((0 0) (1 1) (0 0)))
;; (geom_d '((0 0) (1 0) (2 0) (3 0) (4 0)))
;; (geom_d '((-3 -3) (-1 -1) (2 2) (4 4)))
;; (geom_d '((0 0) (4 0) (0 3)))
;; (geom_d '((0 0) (0 2) (2 0) (2 2)))
;; (geom_d '((-1 -1) (-1 1) (1 -1) (1 1)))
;; (geom_d '((0 0) (6 0) (6 2) (0 2) (3 1)))
;; (geom_d '((0 0) (1 0) (0 1) (-1 0) (0 -1)))
;; (geom_d '((-1 0) (0 0) (1 0) (0 1) (0 -1)))
;; (geom_d '((2 0) (-2 0) (0 2) (0 -2) (1 1) (-1 1) (1 -1) (-1 -1)))
;; (geom_d '((2 0) (1 2) (-1 2) (-2 0) (-1 -2) (1 -2)))
;; (geom_d '((3 0) (2 2) (0 3) (-2 2) (-3 0) (-2 -2) (0 -3) (2 -2)))
;; (geom_d '((0 0) (2 0) (4 0) (1 1) (3 1) (2 2)))
;; (geom_d '((0 0) (0 0) (0 0) (1 1) (1 1) (2 2) (2 2) (3 3)))
;; (geom_d '((0 0) (0 1) (1 0) (1 1) (10 10)))
;; (geom_d '((-10 -10) (-10 10) (10 -10) (10 10) (0 0)))
;; (geom_d '((7 -3) (-2 5) (4 8) (-6 -1) (0 0) (3 -7) (-8 4)))
;; (geom_d '((0 0) (5 1) (2 7) (-3 4) (-6 -2) (8 -5) (1 -8)))
;; (geom_d '((0 0) (100 0) (101 0) (102 0) (50 30) (50 -30)))
;; (geom_d '((0 0) (2 0) (4 0) (6 0) (1 3) (3 3) (5 3)))
;; (geom_d '((0 0) (2 0) (4 0) (1 1) (3 1) (2 3) (2 -3)))
;; (geom_d '((-5 0) (-3 0) (-1 0) (1 0) (3 0) (5 0) (0 2) (0 -2)))
;; (geom_d '((-2 -1) (-1 -2) (1 2) (2 1) (-2 1) (-1 2) (1 -2) (2 -1)))
;; (geom_d '((0 0) (1 2) (2 4) (3 6) (4 8) (5 10) (6 12) (7 14)))
;; (geom_d '((0 0) (1 0) (2 0) (3 0) (0 1) (1 1) (2 1) (3 1) (0 2) (1 2) (2 2) (3 2)))
;; (geom_d '((4 4) (-4 -4) (-4 4) (4 -4) (0 0)))
;------/

; Библиотека для работы с окружностями
;------\
; Проверка: является ли целое число полным квадратом. Возвращает корень или nil
(defun sqrtp_int (n)
    (cond ((< n 0) nil)
          (T (perfect_sqrt_int n (isqrt n)))
    )
)
; Вспомогательная функция для sqrtp_int
(defun perfect_sqrt_int (n r)
    (cond ((= (* r r) n) r)
          (T nil)
    )
)

; Квадратный корень из дроби (только если корень рационален). Возвращает дробь или nil.
(defun sqrt_frac (f) (sqrt_fracc (reduce_frac f)))
; Вспомогательная функция для sqrt_frac
(defun sqrt_fracc (f)
    (cond ((< (num f) 0) nil)
          (T (make_sqrt_frac (sqrtp_int (num f)) (sqrtp_int (den f))))
    )
)
; Вспомогательная функция для sqrt_fracc
(defun make_sqrt_frac (num_sqrt den_sqrt)
    (cond ((and num_sqrt den_sqrt) (create_frac num_sqrt den_sqrt))
          (T nil)
    )
)

; Проверка на то, заданы и различны ли три точки
(defun three_points_ok (p_list)
    (cond ((null p_list) nil)
          ((null (cdr p_list)) nil)
          ((null (cddr p_list)) nil)
          ((null (cdddr p_list)) (cddr (unique_points p_list)))
          (T nil)
    )
)

; Проверка на то, лежат ли три точки на одной прямой
(defun three_points_collinear (p1 p2 p3)
    (zero_frac (sub_frac (mul_frac (sub_frac (get_x p2) (get_x p1)) (sub_frac (get_y p3) (get_y p1)))
                         (mul_frac (sub_frac (get_y p2) (get_y p1)) (sub_frac (get_x p3) (get_x p1)))))
)

; Расстояние между двумя точками (дробь), либо nil если корень не рационален
(defun pair_dist (p1 p2)
    (sqrt_frac (sum_frac (sqr_frac (sub_frac (get_x p1) (get_x p2)))
                         (sqr_frac (sub_frac (get_y p1) (get_y p2)))))
)

; Радиус окружности (дробь), либо nil если корень не рационален
(defun get_r (x y p)
    (sqrt_frac (sum_frac (sqr_frac (sub_frac x (get_x p)))
                         (sqr_frac (sub_frac y (get_y p)))))
)


; Вычисление одной координаты центра окружности
(defun get_center_coordinate (x1 x2 x3 y1 y2 y3)
    (div_frac (sub_frac (mul_frac (sub_frac y2 y1) (sum_frac
                                                        (sub_frac (sqr_frac y3) (sqr_frac y1))
                                                        (sub_frac (sqr_frac x3) (sqr_frac x1))))
                        (mul_frac (sub_frac y3 y1) (sum_frac
                                                        (sub_frac (sqr_frac y2) (sqr_frac y1))
                                                        (sub_frac (sqr_frac x2) (sqr_frac x1)))))
              (mul_frac (init_frac 2) (sub_frac (mul_frac (sub_frac x3 x1) (sub_frac y2 y1))
                                                (mul_frac (sub_frac x2 x1) (sub_frac y3 y1)))))
)

; Построение окружности по списку из трёх точек p_list. Возвращает окружность: ((x y) r) или nil
(defun make_circle (p_list)
    (make_circlee p_list
        (get_center_coordinate
            (get_x (car p_list)) (get_x (cadr p_list)) (get_x (caddr p_list))
            (get_y (car p_list)) (get_y (cadr p_list)) (get_y (caddr p_list)))
        (get_center_coordinate
            (get_y (car p_list)) (get_y (cadr p_list)) (get_y (caddr p_list))
            (get_x (car p_list)) (get_x (cadr p_list)) (get_x (caddr p_list))))
)
; Вспомогательная функция для make_circle
(defun make_circlee (p_list x_cent y_cent)
    (make_circleee p_list x_cent y_cent (get_r x_cent y_cent (car p_list)))
)
; Вспомогательная функция для make_circlee
(defun make_circleee (p_list x_cent y_cent r)
    (cond ((null r) nil)
          (T (list (list x_cent y_cent) r))
    )
)

; Вспомогательная функция, возвращающая список (d a h), где: d - расстояние между центрами, a — проекция расстояния от cent_1 до основания перпендикуляра, h — расстояние от основания до точек пересечения.
(defun helper_inters (x1 y1 r1 x2 y2 r2)
    (helper_inters_1 x1 y1 r1 x2 y2 r2 (pair_dist (list x1 y1) (list x2 y2)))
)
; Вспомогательная функция для helper_inters
(defun helper_inters_1 (x1 y1 r1 x2 y2 r2 d)
    (cond ((null d) nil)
          ((zero_frac d) nil)
          (T (helper_inters_2 r1 r2 d))
    )
)
; Вспомогательная функция для helper_inters_1
(defun helper_inters_2 (r1 r2 d)
    (helper_inters_3 r1 r2 d (div_frac
          (sub_frac (sum_frac (sqr_frac r1) (sqr_frac d)) (sqr_frac r2))
          (mul_frac (init_frac 2) d))
    )
)
; Вспомогательная функция для helper_inters_2
(defun helper_inters_3 (r1 r2 d a) (helper_inters_4 d a (sqrt_frac (sub_frac (sqr_frac r1) (sqr_frac a)))))
; Вспомогательная функция для helper_inters_3
(defun helper_inters_4 (d a h)
    (cond ((null h) nil)
          (T (list d a h))
    )
)

; Точки пересечения двух окружностей. Возвращает список из двух точек (они совпадают при касании) либо nil 
(defun inter_circles (x1 y1 r1 x2 y2 r2) (inter_circles_1 x1 y1 r1 x2 y2 r2 (helper_inters x1 y1 r1 x2 y2 r2)))
; Вспомогательная функция для inter_circles
(defun inter_circles_1 (x1 y1 r1 x2 y2 r2 h)
    (cond ((null h) nil)
          (T (inter_circles_2 x1 y1 x2 y2 (car h) (cadr h) (caddr h)))
    )
)
; Вспомогательная функция для inter_circles_1, строит две точки пересечения по (d a h)
(defun inter_circles_2 (x1 y1 x2 y2 d a h)
    (inter_circles_3 x1 y1 x2 y2 d a h
        (sum_frac x1 (mul_frac (div_frac a d) (sub_frac x2 x1)))
        (sum_frac y1 (mul_frac (div_frac a d) (sub_frac y2 y1)))
    )
)
; Вспомогательная функция для inter_circles_2
(defun inter_circles_3 (x1 y1 x2 y2 d a h x3 y3)
    (list (list (sum_frac x3 (mul_frac (div_frac h d) (sub_frac y2 y1)))
                (sub_frac y3 (mul_frac (div_frac h d) (sub_frac x2 x1))))
          (list (sub_frac x3 (mul_frac (div_frac h d) (sub_frac y2 y1)))
                (sum_frac y3 (mul_frac (div_frac h d) (sub_frac x2 x1)))))
)

; Возвращает строку, если совпадают/вложены/не пересекаются, одну точку, если касаются, две точки, если пересекаются).
(defun e1 (circ_1 circ_2)
    (e1_activate circ_1 circ_2 (pair_dist (car circ_1) (car circ_2)))
)
; Вспомогательная функция для e1
(defun e1_activate (circ_1 circ_2 d)
    (cond
        ((null d) "Невозможно определить отношение окружностей: расстояние не выражается рационально")
        ((and (zero_frac d) (eq_frac (cadr circ_1) (cadr circ_2))) "Окружности совпадают")
        ((< (cmp_frac d (abs_frac (sub_frac (cadr circ_1) (cadr circ_2)))) 0) "Одна окружность полностью лежит внутри другой")
        ((> (cmp_frac d (sum_frac (cadr circ_1) (cadr circ_2))) 0) "Окружности не пересекаются и не вложены друг в друга")
        ((or (zero_frac (sub_frac d (sum_frac (cadr circ_1) (cadr circ_2))))
             (zero_frac (sub_frac d (abs_frac (sub_frac (cadr circ_1) (cadr circ_2))))))
         (car (inter_circles (get_x (car circ_1)) (get_y (car circ_1)) (cadr circ_1)
                             (get_x (car circ_2)) (get_y (car circ_2)) (cadr circ_2))))
        (T (inter_circles (get_x (car circ_1)) (get_y (car circ_1)) (cadr circ_1)
                          (get_x (car circ_2)) (get_y (car circ_2)) (cadr circ_2)))
    )
)

; Проверяет, лежит ли центр одной окружности внутри другой
(defun e2 (circ_1 circ_2) (e2_activate circ_1 circ_2 (pair_dist (car circ_1) (car circ_2))))
; Вспомогательная функция для e2
(defun e2_activate (circ_1 circ_2 d)
    (cond ((null d) "Невозможно определить: расстояние между центрами не выражается рационально")
          ((and (< (cmp_frac d (cadr circ_1)) 0) (< (cmp_frac d (cadr circ_2)) 0)) "Оба центра лежат в других окружностях")
          ((or  (< (cmp_frac d (cadr circ_1)) 0) (< (cmp_frac d (cadr circ_2)) 0)) "Центр одной окружности лежит внутри другой")
          (T "Ни один из центров не лежит внутри другой окружности")
    )
)

; Строит третью окружность минимального радиуса, касающуюся двух данных снаружи, если окружности не пересекаются и не вложены (d > r1 + r2).
(defun third_circle (circ_1 circ_2) (third_circle_1 circ_1 circ_2 (pair_dist (car circ_1) (car circ_2))))
; Вспомогательная функция для third_circle
(defun third_circle_1 (circ_1 circ_2 d)
    (cond ((null d) "Окружности не располагаются так, чтобы можно было построить подходящую окружность!")
          ((> (cmp_frac d (sum_frac (cadr circ_1) (cadr circ_2))) 0)
           (third_circle_2 circ_1 circ_2 d (div_frac (sub_frac d (sum_frac (cadr circ_1) (cadr circ_2))) (init_frac 2))))
          (T "Окружности не располагаются так, чтобы можно было построить подходящую окружность!")
    )
)
; Вспомогательная функция для third_circle_1
(defun third_circle_2 (circ_1 circ_2 d r3) (third_circle_3 circ_1 circ_2 d r3 (div_frac (sum_frac (cadr circ_1) r3) d)))
; Вспомогательная функция для third_circle_2
(defun third_circle_3 (circ_1 circ_2 d r3 proec)
    (list (list (sum_frac (get_x (car circ_1)) (mul_frac proec (sub_frac (get_x (car circ_2)) (get_x (car circ_1)))))
                (sum_frac (get_y (car circ_1)) (mul_frac proec (sub_frac (get_y (car circ_2)) (get_y (car circ_1)))))) 
          r3)
)

; Точки касания третьей окружности с исходными (при касании пересечение даёт две одинаковые точки)
(defun third_circle_inter (circ_1 circ_2 circ_3)
    (cond ((and (listp circ_3) (listp (car circ_3)))
           (list (car (inter_circles (get_x (car circ_1)) (get_y (car circ_1)) (cadr circ_1)
                                     (get_x (car circ_3)) (get_y (car circ_3)) (cadr circ_3)))
                 (car (inter_circles (get_x (car circ_2)) (get_y (car circ_2)) (cadr circ_2)
                                     (get_x (car circ_3)) (get_y (car circ_3)) (cadr circ_3)))
           ))
          (T "Нельзя определить точки касания - 3-й окружности нет!")
    )
)

; Печатает вывод
(defun print_val (v)
    (cond ((null v) (princ "ОШИБКА: значение не вычислено") (terpri))
          ((stringp v) (princ v) (terpri))
          ((and (listp v) (listp (car v)) (listp (cadr v)) (fracp (caar v)))
           (princ "Точки:") (terpri) (print_points v))
          ((and (listp v) (fracp (car v)) (fracp (cadr v)))
           (print_point v) (terpri))
          (T (princ v) (terpri))
    )
)

(defun fracp (obj)
    (cond ((and (consp obj) (consp (cdr obj)) (null (cddr obj)) (integerp (car obj)) (integerp (cadr obj))) T)
          (T nil)
    )
)

; Функция активации пункта e
(defun geom_e (p_list) (activate_geom_e (normalize_points p_list)))
; Вспомогательная функция с накапливающим параметром для geom_e
(defun activate_geom_e (p_list)
    (princ "Исходные точки:") (terpri)
    (print_points p_list) (terpri)
    (princ "==============================") (terpri)
    (princ "ПУНКТ e: построение двух окружностей по трём точкам") (terpri)
    (cond ((or (null p_list) (null (cdddr p_list)) (null (cddddr p_list)) (null (cdr (cddddr p_list))))
           (princ "ОШИБКА: требуется список из 6 точек: ((x1 y1) (x2 y2) (x3 y3) (x4 y4) (x5 y5) (x6 y6))") (terpri))
          (T (e0 (list (car p_list) (cadr p_list) (caddr p_list))
                                    (list (cadddr p_list) (car (cddddr p_list)) (cadr (cddddr p_list)))
                                    (e0_try (list (car p_list) (cadr p_list) (caddr p_list)) 1)
                                    (e0_try (list (cadddr p_list) (car (cddddr p_list)) (cadr (cddddr p_list))) 2)
         ))
    )
    (princ "==============================") (terpri)
)

; Строит окружность (или nil) и печатает диагностическое сообщение
(defun e0_try (p3 idx)
    (cond ((not (three_points_ok p3))
                (cond ((= idx 1) (princ "ОШИБКА: первую окружность построить невозможно из-за совпадающих точек!") (terpri))
                      (T (princ "ОШИБКА: вторую окружность построить невозможно из-за совпадающих точек!") (terpri))) nil)
          ((three_points_collinear (car p3) (cadr p3) (caddr p3))
                (cond ((= idx 1) (princ "ОШИБКА: первую окружность построить невозможно: точки лежат на одной прямой!") (terpri))
                      (T (princ "ОШИБКА: вторую окружность построить невозможно: точки лежат на одной прямой!") (terpri))) nil)
          (T (e0_try_1 (make_circle p3) idx))
    )
)
; Вспомогательная функция для e0_try: если радиус не рационален — считаем окружность некорректной
(defun e0_try_1 (circ idx)
    (cond ((null circ)
          (cond ((= idx 1) (princ "ОШИБКА: первую окружность построить невозможно: радиус не выражается рационально!") (terpri))
                (T (princ "ОШИБКА: вторую окружность построить невозможно: радиус не выражается рационально!") (terpri))) nil)
          (T circ)
    )
)

; Функция активации подпункта e0 - построение двух окружностей
(defun e0 (p3_1 p3_2 cent_1 cent_2)
    (cond ((or (null cent_1) (null cent_2))
           (princ "ОШИБКА: подзадачи e1, e2 и e3 решить нельзя — есть некорректные окружности!") (terpri))
          (T (princ "Окружность 1:") (terpri) (print_circle cent_1)
             (princ "Окружность 2:") (terpri) (print_circle cent_2)
             (princ "==============================") (terpri)
             (princ "ПОДПУНКТ e1: взаимное расположение окружностей") (terpri)
             (print_val (e1 cent_1 cent_2)) (terpri)
             (princ "==============================") (terpri)
             (princ "ПОДПУНКТ e2: проверка центров (лежит ли центр одной окружности внутри другой)") (terpri)
             (print_val (e2 cent_1 cent_2))
             (princ "==============================") (terpri)
             (princ "ПОДПУНКТ e3: третья окружность минимального радиуса, касающаяся обеих") (terpri)
             (e3 cent_1 cent_2))
    )
)

; Функция активации подпункта e3 - третья окружность с минимальным радиусом
(defun e3 (cent_1 cent_2) (e3_activate cent_1 cent_2 (third_circle cent_1 cent_2)))
; Вспомогательная функция с накапливающим параметром для e3 
(defun e3_activate (cent_1 cent_2 cent_3)
    (cond ((and (listp cent_3) (listp (car cent_3)))
           (princ "Третья окружность:") (terpri) (print_circle cent_3)
           (princ "Точки касания:") (terpri)
           (print_val (third_circle_inter cent_1 cent_2 cent_3)))
         (T (print_val cent_3))
    )
)
;------/

; Тесты для пункта e
;------\
;; (geom_e '((6 2) (1 7) (-4 2) (12 2) (7 7) (2 2)))
;; (geom_e '((3 0) (0 3) (-3 0) (7 0) (5 2) (3 0)))
;; (geom_e '((5 0) (0 5) (-5 0) (5 0) (3 2) (1 0)))
;; (geom_e '((3 0) (0 3) (-3 0) (12 0) (10 2) (8 0)))
;; (geom_e '((2 0) (0 2) (-2 0) (8 8) (6 10) (4 8)))
(geom_e '((5 0) (0 5) (-5 0) (2 1) (1 2) (0 1)))
;; (geom_e '((5 0) (0 5) (-5 0) (2 0) (0 2) (-2 0)))
;; (geom_e '((5 0) (0 5) (-5 0) (0 5) (-5 0) (0 -5)))
;; (geom_e '((5 0) (0 5) (-5 0) (7 0) (3 4) (-1 0)))
;; (geom_e '((5 0) (0 5) (-5 0) (7 0) (4 3) (1 0)))
;; (geom_e '((0 0) (1 1) (2 2) (5 0) (0 5) (-5 0)))
;; (geom_e '((5 0) (0 5) (-5 0) (0 0) (0 0) (1 0)))
;; (geom_e '((5 0) (0 5) (-5 0) (1 1) (1 1) (1 1)))
;; (geom_e '((0 0) (0 0) (1 0) (5 0) (0 5) (-5 0)))
;; (geom_e '((5 0) (0 5) (-5 0) (-13 0) (-8 5) (-3 0)))
;; (geom_e '((-1 -3) (-3 -1) (-5 -3) (6 -1) (5 0) (4 -1)))
;; (geom_e '((-1 1) (0 0) (-1 -1) (1 0) (2 1) (3 0)))
;------/

;------\
(defun on_the_line (data) (print_lines (a4 (car (normalize_points data)) (get_lines (cdr (normalize_points data)) nil) nil)))
(defun a4 (p l_list ans)
    (cond ((null l_list) ans)
          ((check_line p (car l_list)) (a4 p (cdr l_list) (add_line (car l_list) ans)))
          (T (a4 p (cdr l_list) ans))
    )
)
(defun check_line (p cur_l)
    (cond ((eq_frac (init_frac 0) (sum_frac (sum_frac (mul_frac (get_A cur_l) (get_x p)) (mul_frac (get_B cur_l) (get_y p))) (get_C cur_l))) T))
)

(on_the_line '((2 2) (0 0) (1 1) (2 2) (3 3) (0 4)))
;------/