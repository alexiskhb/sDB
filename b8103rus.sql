CREATE DATABASE 'localhost:/home/alexiskhb/DB/sDB/b8103rus.fdb' user 'SYSDBA' password '20052008'
 default character set UTF8;

CREATE SEQUENCE global_sequence;
ALTER SEQUENCE global_sequence RESTART WITH 10000;

CREATE TABLE teachers(
	id integer primary key,
	name varchar(50) not null
	);

CREATE TABLE groups(
	id integer primary key,
	name varchar(50) unique not null
	);

CREATE TABLE courses(
	id integer primary key,
	name varchar(50) unique not null
	);

CREATE TABLE classrooms(
	id integer primary key,
	classroom varchar(50) unique not null
	);

CREATE TABLE weekdays (
	id integer primary key,
	Weekday varchar(15) unique not null
	);

CREATE TABLE pairs(
	id integer primary key,
	period varchar(50) unique not null
	);

CREATE TABLE groups_courses(
	id integer primary key,
	group_id integer references groups(id),
	course_id integer references courses(id),
  constraint unique_g_c_relation unique (group_id, course_id)
	);
	
CREATE TABLE teachers_courses(
	id integer primary key,
	teacher_id integer references teachers(id),
	course_id integer references courses(id),
  constraint unique_t_c_relation unique (teacher_id, course_id)
	);

CREATE TABLE lessons(
	id integer primary key,
	pair_id integer references pairs(id),
	weekday_id integer references weekdays(id),
	group_id integer references groups(id), 
	course_id integer references courses(id),
	class_id integer references classrooms(id),
	teacher_id integer references teachers(id),
  constraint unique_lessons unique (pair_id, weekday_id, group_id, course_id, class_id, teacher_id)
	);

INSERT INTO teachers VALUES (100, 'Жуплев Антон Сергеевич');
INSERT INTO teachers VALUES (101, 'Спорышев Максим Сергеевич');
INSERT INTO teachers VALUES (102, 'Пак Геннадий Константинович');
INSERT INTO teachers VALUES (103, 'Клевчихин Юрий Александрович');
INSERT INTO teachers VALUES (104, 'Кленин Александр Сергеевич');
INSERT INTO teachers VALUES (105, 'Лудов Игорь Юрьевич');
INSERT INTO teachers VALUES (106, 'Машенцев Владимир Юрьевич');
INSERT INTO teachers VALUES (107, 'Никольская Татьяна Владимировна');
INSERT INTO teachers VALUES (108, 'Одинов Общеслав Физрукович');
INSERT INTO teachers VALUES (109, 'Давыдов Денис Витальевич');
INSERT INTO teachers VALUES (110, 'Достовалов Валерий Николаевич');
INSERT INTO teachers VALUES (111, 'Шепелева Риорита Петровна');
INSERT INTO teachers VALUES (112, 'Романюк Мария Александровна');
INSERT INTO teachers VALUES (113, 'Пак Татьяна Владимировна');
INSERT INTO teachers VALUES (114, 'Брижитский Роман Викторович');
INSERT INTO teachers VALUES (115, 'Пинько Ирина Викторовна');
INSERT INTO teachers VALUES (116, 'Кравцов Дмитрий Сергеевич');

INSERT INTO groups VALUES (200, 'Б8103А1');
INSERT INTO groups VALUES (201, 'Б8103А2');
INSERT INTO groups VALUES (202, 'Б8203А1');
INSERT INTO groups VALUES (203, 'Б8203А2');

INSERT INTO courses VALUES (300, 'Алгебра и геометрия');
INSERT INTO courses VALUES (301, 'Математический анализ');
INSERT INTO courses VALUES (302, 'Практика на ЭВМ');
INSERT INTO courses VALUES (303, 'ЯиМП');
INSERT INTO courses VALUES (304, 'Физкультура');
INSERT INTO courses VALUES (305, 'Английский язык');
INSERT INTO courses VALUES (306, 'Дискретная математика');
INSERT INTO courses VALUES (307, 'Базы данных');
INSERT INTO courses VALUES (308, 'Экономика');
INSERT INTO courses VALUES (309, 'Объектно-ориентированный анализ');
INSERT INTO courses VALUES (310, 'Физика');
INSERT INTO courses VALUES (311, 'Дифференциальные уравнения');
INSERT INTO courses VALUES (312, 'Комплексный анализ');
INSERT INTO courses VALUES (313, 'Численный методы');
INSERT INTO courses VALUES (314, 'ОБЖ');
INSERT INTO courses VALUES (315, 'Экономическая теория');
INSERT INTO courses VALUES (316, 'Биология');

INSERT INTO classrooms VALUES (401, 'D734а');
INSERT INTO classrooms VALUES (402, 'D734б');
INSERT INTO classrooms VALUES (403, 'D743');
INSERT INTO classrooms VALUES (404, 'D547');
INSERT INTO classrooms VALUES (405, 'D542');
INSERT INTO classrooms VALUES (406, 'D732');
INSERT INTO classrooms VALUES (407, 'D738');
INSERT INTO classrooms VALUES (408, 'D549');
INSERT INTO classrooms VALUES (409, 'D409');
INSERT INTO classrooms VALUES (410, 'D410');
INSERT INTO classrooms VALUES (411, 'D411');
INSERT INTO classrooms VALUES (412, 'D412');
INSERT INTO classrooms VALUES (413, 'D413');
INSERT INTO classrooms VALUES (414, 'D414');
INSERT INTO classrooms VALUES (415, 'Спортивный корпус');

INSERT INTO weekdays VALUES (501, 'Понедельник');
INSERT INTO weekdays VALUES (502, 'Вторник');
INSERT INTO weekdays VALUES (503, 'Среда');
INSERT INTO weekdays VALUES (504, 'Четверг');
INSERT INTO weekdays VALUES (505, 'Пятница');
INSERT INTO weekdays VALUES (506, 'Суббота');
INSERT INTO weekdays VALUES (507, 'Воскресенье');

INSERT INTO pairs VALUES (1, '8:30-10:00');
INSERT INTO pairs VALUES (2, '10:10-11:40');
INSERT INTO pairs VALUES (3, '11:50-13:20');
INSERT INTO pairs VALUES (4, '13:30-15:00');
INSERT INTO pairs VALUES (5, '15:10-16:40');
INSERT INTO pairs VALUES (6, '16:50-18:20');
INSERT INTO pairs VALUES (7, '18:30-20:00');
INSERT INTO pairs VALUES (8, '20:10-21:40');
INSERT INTO pairs VALUES (9, '21:50-23:20');

INSERT INTO groups_courses VALUES (700, 200, 300);
INSERT INTO groups_courses VALUES (701, 200, 301);
INSERT INTO groups_courses VALUES (702, 200, 302);
INSERT INTO groups_courses VALUES (703, 200, 303);
INSERT INTO groups_courses VALUES (704, 200, 304);
INSERT INTO groups_courses VALUES (705, 200, 305);
INSERT INTO groups_courses VALUES (706, 200, 306);
INSERT INTO groups_courses VALUES (707, 200, 307);
INSERT INTO groups_courses VALUES (708, 201, 300);
INSERT INTO groups_courses VALUES (709, 201, 301);
INSERT INTO groups_courses VALUES (710, 201, 302);
INSERT INTO groups_courses VALUES (711, 201, 303);
INSERT INTO groups_courses VALUES (712, 201, 304);
INSERT INTO groups_courses VALUES (713, 201, 305);
INSERT INTO groups_courses VALUES (714, 201, 306);
INSERT INTO groups_courses VALUES (715, 201, 307);
INSERT INTO groups_courses VALUES (716, 202, 300);
INSERT INTO groups_courses VALUES (717, 202, 301);
INSERT INTO groups_courses VALUES (718, 202, 302);
INSERT INTO groups_courses VALUES (719, 202, 303);
INSERT INTO groups_courses VALUES (720, 202, 304);
INSERT INTO groups_courses VALUES (721, 202, 305);
INSERT INTO groups_courses VALUES (722, 202, 306);
INSERT INTO groups_courses VALUES (723, 202, 307);
INSERT INTO groups_courses VALUES (724, 202, 308);
INSERT INTO groups_courses VALUES (725, 202, 309);
INSERT INTO groups_courses VALUES (726, 202, 311);
INSERT INTO groups_courses VALUES (727, 202, 312);
INSERT INTO groups_courses VALUES (728, 202, 313);
INSERT INTO groups_courses VALUES (729, 202, 314);
INSERT INTO groups_courses VALUES (730, 202, 315);
INSERT INTO groups_courses VALUES (731, 203, 300);
INSERT INTO groups_courses VALUES (732, 203, 301);
INSERT INTO groups_courses VALUES (733, 203, 302);
INSERT INTO groups_courses VALUES (734, 203, 303);
INSERT INTO groups_courses VALUES (735, 203, 304);
INSERT INTO groups_courses VALUES (736, 203, 305);
INSERT INTO groups_courses VALUES (737, 203, 306);
INSERT INTO groups_courses VALUES (738, 203, 307);
INSERT INTO groups_courses VALUES (739, 203, 308);
INSERT INTO groups_courses VALUES (740, 203, 309);
INSERT INTO groups_courses VALUES (741, 203, 310);
INSERT INTO groups_courses VALUES (742, 203, 311);
INSERT INTO groups_courses VALUES (743, 203, 312);
INSERT INTO groups_courses VALUES (744, 203, 313);
INSERT INTO groups_courses VALUES (745, 203, 314);

INSERT INTO teachers_courses VALUES (800, 100, 302);
INSERT INTO teachers_courses VALUES (801, 101, 303);
INSERT INTO teachers_courses VALUES (802, 102, 300);
INSERT INTO teachers_courses VALUES (803, 103, 301);
INSERT INTO teachers_courses VALUES (804, 104, 307);
INSERT INTO teachers_courses VALUES (805, 105, 303);
INSERT INTO teachers_courses VALUES (806, 106, 302);
INSERT INTO teachers_courses VALUES (807, 107, 305);
INSERT INTO teachers_courses VALUES (808, 108, 304);
INSERT INTO teachers_courses VALUES (809, 109, 308);
INSERT INTO teachers_courses VALUES (810, 100, 309);
INSERT INTO teachers_courses VALUES (811, 111, 310);
INSERT INTO teachers_courses VALUES (812, 112, 311);
INSERT INTO teachers_courses VALUES (813, 113, 313);
INSERT INTO teachers_courses VALUES (814, 114, 314);
INSERT INTO teachers_courses VALUES (815, 115, 315);
INSERT INTO teachers_courses VALUES (816, 116, 305);
--Первые 2 группы
--Понедельник
INSERT INTO lessons VALUES(900, 1, 501, 200, 302, 401, 100);
INSERT INTO lessons VALUES(901, 1, 501, 201, 302, 402, 106);
INSERT INTO lessons VALUES(902, 2, 501, 200, 302, 401, 100);
INSERT INTO lessons VALUES(903, 2, 501, 201, 302, 402, 106);
INSERT INTO lessons VALUES(904, 3, 501, 200, 303, 408, 101);
INSERT INTO lessons VALUES(905, 4, 501, 200, 303, 408, 101);
--Вторник
INSERT INTO lessons VALUES(906, 1, 502, 200, 300, 401, 102);
INSERT INTO lessons VALUES(907, 1, 502, 201, 300, 401, 102);
INSERT INTO lessons VALUES(908, 2, 502, 200, 306, 402, 102);
INSERT INTO lessons VALUES(909, 2, 502, 201, 306, 402, 102);
INSERT INTO lessons VALUES(910, 3, 502, 200, 301, 403, 103);
INSERT INTO lessons VALUES(911, 3, 502, 201, 301, 403, 103);
INSERT INTO lessons VALUES(912, 4, 502, 200, 306, 401, 102);
INSERT INTO lessons VALUES(913, 4, 502, 201, 306, 401, 102);
--Среда
INSERT INTO lessons VALUES(914, 2, 503, 200, 306, 404, 102);
INSERT INTO lessons VALUES(915, 2, 503, 201, 306, 404, 102);
INSERT INTO lessons VALUES(916, 3, 503, 200, 300, 404, 102);
INSERT INTO lessons VALUES(917, 3, 503, 201, 300, 404, 102);
INSERT INTO lessons VALUES(918, 4, 503, 200, 300, 405, 102);
INSERT INTO lessons VALUES(919, 4, 503, 201, 300, 405, 102);
INSERT INTO lessons VALUES(920, 5, 503, 200, 304, 404, 108);
INSERT INTO lessons VALUES(921, 5, 503, 201, 304, 404, 108);
--Четверг
INSERT INTO lessons VALUES(922, 1, 504, 201, 303, 401, 101);
INSERT INTO lessons VALUES(923, 2, 504, 201, 303, 401, 101);
INSERT INTO lessons VALUES(924, 3, 504, 200, 303, 407, 105);
INSERT INTO lessons VALUES(925, 3, 504, 201, 303, 407, 105);
--Пятница
INSERT INTO lessons VALUES(926, 2, 505, 200, 301, 405, 103);
INSERT INTO lessons VALUES(927, 2, 505, 201, 301, 405, 103);
INSERT INTO lessons VALUES(928, 3, 505, 200, 301, 405, 103);
INSERT INTO lessons VALUES(929, 3, 505, 201, 301, 405, 103);
--Суббота
INSERT INTO lessons VALUES(930, 1, 506, 200, 307, 402, 104);
INSERT INTO lessons VALUES(931, 2, 506, 201, 307, 402, 104);
INSERT INTO lessons VALUES(932, 3, 506, 201, 307, 405, 104);
INSERT INTO lessons VALUES(933, 3, 506, 200, 307, 405, 104);
INSERT INTO lessons VALUES(934, 4, 506, 200, 305, 406, 107);
INSERT INTO lessons VALUES(935, 4, 506, 201, 305, 406, 107);
INSERT INTO lessons VALUES(936, 5, 506, 200, 304, 415, 108);
INSERT INTO lessons VALUES(937, 5, 506, 201, 304, 415, 108);
--Вторые 2 группы
--Понедельник
INSERT INTO lessons VALUES(938, 1, 501, 202, 308, 405, 109);
INSERT INTO lessons VALUES(939, 1, 501, 203, 308, 405, 109);
INSERT INTO lessons VALUES(940, 2, 501, 202, 308, 405, 109);
INSERT INTO lessons VALUES(941, 2, 501, 203, 308, 405, 109);
INSERT INTO lessons VALUES(942, 3, 501, 202, 309, 406, 100);
INSERT INTO lessons VALUES(943, 3, 501, 203, 309, 406, 100);
INSERT INTO lessons VALUES(944, 4, 501, 202, 302, 407, 104);
INSERT INTO lessons VALUES(945, 4, 501, 203, 309, 408, 100);
INSERT INTO lessons VALUES(946, 5, 501, 202, 302, 407, 104);
INSERT INTO lessons VALUES(947, 5, 501, 203, 309, 408, 100);
--Вторник
INSERT INTO lessons VALUES(948, 5, 502, 202, 304, 414, 108);
INSERT INTO lessons VALUES(949, 5, 502, 203, 304, 414, 108);
--Среда
INSERT INTO lessons VALUES(950, 1, 503, 202, 310, 406, 110);
INSERT INTO lessons VALUES(951, 1, 503, 203, 310, 406, 110);
INSERT INTO lessons VALUES(952, 2, 503, 202, 311, 405, 111);
INSERT INTO lessons VALUES(953, 2, 503, 203, 311, 405, 111);
INSERT INTO lessons VALUES(954, 3, 503, 202, 311, 405, 111);
INSERT INTO lessons VALUES(955, 3, 503, 203, 311, 405, 111);
INSERT INTO lessons VALUES(956, 4, 503, 202, 310, 407, 110);
INSERT INTO lessons VALUES(957, 4, 503, 203, 310, 407, 110);
INSERT INTO lessons VALUES(958, 5, 503, 202, 305, 406, 112);
INSERT INTO lessons VALUES(959, 5, 503, 203, 305, 406, 112);
--Четверг
INSERT INTO lessons VALUES(960, 1, 504, 202, 312, 405, 103);
INSERT INTO lessons VALUES(961, 1, 504, 203, 312, 405, 103);
INSERT INTO lessons VALUES(962, 2, 504, 202, 313, 406, 113);
INSERT INTO lessons VALUES(963, 2, 504, 203, 313, 406, 113);
INSERT INTO lessons VALUES(964, 3, 504, 202, 312, 407, 103);
INSERT INTO lessons VALUES(965, 3, 504, 203, 312, 407, 103);
INSERT INTO lessons VALUES(966, 4, 504, 202, 313, 408, 114);
INSERT INTO lessons VALUES(967, 4, 504, 203, 313, 408, 114);
INSERT INTO lessons VALUES(968, 5, 504, 202, 314, 409, 115);
INSERT INTO lessons VALUES(969, 5, 504, 203, 314, 409, 115);
--Пятница
INSERT INTO lessons VALUES(970, 1, 505, 202, 309, 405, 100);
INSERT INTO lessons VALUES(971, 1, 505, 203, 302, 405, 104);
INSERT INTO lessons VALUES(972, 2, 505, 202, 309, 405, 100);
INSERT INTO lessons VALUES(973, 2, 505, 203, 302, 405, 104);
INSERT INTO lessons VALUES(974, 3, 505, 202, 315, 406, 116);
INSERT INTO lessons VALUES(975, 3, 505, 203, 315, 406, 116);
INSERT INTO lessons VALUES(976, 5, 505, 202, 304, 414, 108);
INSERT INTO lessons VALUES(977, 5, 505, 203, 304, 414, 108);