CREATE DATABASE 'localhost:/home/alexiskhb/DB/sDB/b8103rus.fdb' user 'SYSDBA' password '20052008'
 default character set UTF8;

CREATE TABLE teachers(
	id integer primary key,
	name varchar(50)
	);

CREATE TABLE groups(
	id integer primary key,
	name varchar(50)
	);

CREATE TABLE courses(
	id integer primary key,
	name varchar(50)
	);

CREATE TABLE classrooms(
	id integer primary key,
	classroom varchar(50)
	);

CREATE TABLE weekdays (
	id integer primary key,
	Weekday varchar(15)
	);

CREATE TABLE pairs(
	id integer primary key,
	period varchar(50)
	);

CREATE TABLE groups_courses(
	group_id integer references groups(id),
	course_id integer references courses(id)
	);
	
CREATE TABLE teachers_courses(
	teacher_id integer references teachers(id),
	course_id integer references courses(id)
	);

CREATE TABLE lessons(
	pair_id integer references pairs(id),
	weekday_id integer references weekdays(id),
	group_id integer references groups(id), 
	course_id integer references courses(id),
	class_id integer references classrooms(id),
	teacher_id integer references teachers(id)
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

INSERT INTO pairs VALUES (1, '1. 8:30-10:00');
INSERT INTO pairs VALUES (2, '2. 10:10-11:40');
INSERT INTO pairs VALUES (3, '3. 11:50-13:20');
INSERT INTO pairs VALUES (4, '4. 13:30-15:00');
INSERT INTO pairs VALUES (5, '5. 15:10-16:40');
INSERT INTO pairs VALUES (6, '6. 16:50-18:20');
INSERT INTO pairs VALUES (7, '7. 18:30-20:00');
INSERT INTO pairs VALUES (8, '8. 20:10-21:40');
INSERT INTO pairs VALUES (9, '9. 21:50-23:20');

INSERT INTO groups_courses VALUES (200, 300);
INSERT INTO groups_courses VALUES (200, 301);
INSERT INTO groups_courses VALUES (200, 302);
INSERT INTO groups_courses VALUES (200, 303);
INSERT INTO groups_courses VALUES (200, 304);
INSERT INTO groups_courses VALUES (200, 305);
INSERT INTO groups_courses VALUES (200, 306);
INSERT INTO groups_courses VALUES (200, 307);
INSERT INTO groups_courses VALUES (201, 300);
INSERT INTO groups_courses VALUES (201, 301);
INSERT INTO groups_courses VALUES (201, 302);
INSERT INTO groups_courses VALUES (201, 303);
INSERT INTO groups_courses VALUES (201, 304);
INSERT INTO groups_courses VALUES (201, 305);
INSERT INTO groups_courses VALUES (201, 306);
INSERT INTO groups_courses VALUES (201, 307);
INSERT INTO groups_courses VALUES (202, 300);
INSERT INTO groups_courses VALUES (202, 301);
INSERT INTO groups_courses VALUES (202, 302);
INSERT INTO groups_courses VALUES (202, 303);
INSERT INTO groups_courses VALUES (202, 304);
INSERT INTO groups_courses VALUES (202, 305);
INSERT INTO groups_courses VALUES (202, 306);
INSERT INTO groups_courses VALUES (202, 307);
INSERT INTO groups_courses VALUES (202, 308);
INSERT INTO groups_courses VALUES (202, 309);
INSERT INTO groups_courses VALUES (202, 311);
INSERT INTO groups_courses VALUES (202, 312);
INSERT INTO groups_courses VALUES (202, 313);
INSERT INTO groups_courses VALUES (202, 314);
INSERT INTO groups_courses VALUES (202, 315);
INSERT INTO groups_courses VALUES (203, 300);
INSERT INTO groups_courses VALUES (203, 301);
INSERT INTO groups_courses VALUES (203, 302);
INSERT INTO groups_courses VALUES (203, 303);
INSERT INTO groups_courses VALUES (203, 304);
INSERT INTO groups_courses VALUES (203, 305);
INSERT INTO groups_courses VALUES (203, 306);
INSERT INTO groups_courses VALUES (203, 307);
INSERT INTO groups_courses VALUES (203, 308);
INSERT INTO groups_courses VALUES (203, 309);
INSERT INTO groups_courses VALUES (203, 310);
INSERT INTO groups_courses VALUES (203, 311);
INSERT INTO groups_courses VALUES (203, 312);
INSERT INTO groups_courses VALUES (203, 313);
INSERT INTO groups_courses VALUES (203, 314);

INSERT INTO teachers_courses VALUES (100, 302);
INSERT INTO teachers_courses VALUES (101, 303);
INSERT INTO teachers_courses VALUES (102, 300);
INSERT INTO teachers_courses VALUES (103, 301);
INSERT INTO teachers_courses VALUES (104, 307);
INSERT INTO teachers_courses VALUES (105, 303);
INSERT INTO teachers_courses VALUES (106, 302);
INSERT INTO teachers_courses VALUES (107, 305);
INSERT INTO teachers_courses VALUES (108, 304);
INSERT INTO teachers_courses VALUES (109, 308);
INSERT INTO teachers_courses VALUES (100, 309);
INSERT INTO teachers_courses VALUES (111, 310);
INSERT INTO teachers_courses VALUES (112, 311);
INSERT INTO teachers_courses VALUES (113, 313);
INSERT INTO teachers_courses VALUES (114, 314);
INSERT INTO teachers_courses VALUES (115, 315);
INSERT INTO teachers_courses VALUES (116, 305);

--Первые 2 группы
--Понедельник
INSERT INTO lessons VALUES(1, 501, 200, 302, 401, 100);
INSERT INTO lessons VALUES(1, 501, 201, 302, 402, 106);
INSERT INTO lessons VALUES(2, 501, 200, 302, 401, 100);
INSERT INTO lessons VALUES(2, 501, 201, 302, 402, 106);
INSERT INTO lessons VALUES(3, 501, 200, 303, 408, 101);
INSERT INTO lessons VALUES(4, 501, 200, 303, 408, 101);

--Вторник
INSERT INTO lessons VALUES(1, 502, 200, 300, 401, 102);
INSERT INTO lessons VALUES(1, 502, 201, 300, 401, 102);
INSERT INTO lessons VALUES(2, 502, 200, 306, 402, 102);
INSERT INTO lessons VALUES(2, 502, 201, 306, 402, 102);
INSERT INTO lessons VALUES(3, 502, 200, 301, 403, 103);
INSERT INTO lessons VALUES(3, 502, 201, 301, 403, 103);
INSERT INTO lessons VALUES(4, 502, 200, 306, 401, 102);
INSERT INTO lessons VALUES(4, 502, 201, 306, 401, 102);

--Среда
INSERT INTO lessons VALUES(2, 503, 200, 306, 404, 102);
INSERT INTO lessons VALUES(2, 503, 201, 306, 404, 102);
INSERT INTO lessons VALUES(3, 503, 200, 300, 404, 102);
INSERT INTO lessons VALUES(3, 503, 201, 300, 404, 102);
INSERT INTO lessons VALUES(4, 503, 200, 300, 405, 102);
INSERT INTO lessons VALUES(4, 503, 201, 300, 405, 102);
INSERT INTO lessons VALUES(5, 503, 200, 304, 404, 108);
INSERT INTO lessons VALUES(5, 503, 201, 304, 404, 108);

--Четверг
INSERT INTO lessons VALUES(1, 504, 201, 303, 401, 101);
INSERT INTO lessons VALUES(2, 504, 201, 303, 401, 101);
INSERT INTO lessons VALUES(3, 504, 200, 303, 407, 105);
INSERT INTO lessons VALUES(3, 504, 201, 303, 407, 105);

--Пятница
INSERT INTO lessons VALUES(2, 505, 200, 301, 405, 103);
INSERT INTO lessons VALUES(2, 505, 201, 301, 405, 103);
INSERT INTO lessons VALUES(3, 505, 200, 301, 405, 103);
INSERT INTO lessons VALUES(3, 505, 201, 301, 405, 103);

--Суббота
INSERT INTO lessons VALUES(1, 506, 200, 307, 402, 104);
INSERT INTO lessons VALUES(2, 506, 201, 307, 402, 104);
INSERT INTO lessons VALUES(3, 506, 201, 307, 405, 104);
INSERT INTO lessons VALUES(3, 506, 200, 307, 405, 104);
INSERT INTO lessons VALUES(4, 506, 200, 305, 406, 107);
INSERT INTO lessons VALUES(4, 506, 201, 305, 406, 107);
INSERT INTO lessons VALUES(5, 506, 200, 304, 415, 108);
INSERT INTO lessons VALUES(5, 506, 201, 304, 415, 108);


--Вторые 2 группы
--Понедельник
INSERT INTO lessons VALUES(1, 501, 202, 308, 405, 109);
INSERT INTO lessons VALUES(1, 501, 203, 308, 405, 109);
INSERT INTO lessons VALUES(2, 501, 202, 308, 405, 109);
INSERT INTO lessons VALUES(2, 501, 203, 308, 405, 109);
INSERT INTO lessons VALUES(3, 501, 202, 309, 406, 100);
INSERT INTO lessons VALUES(3, 501, 203, 309, 406, 100);
INSERT INTO lessons VALUES(4, 501, 202, 302, 407, 104);
INSERT INTO lessons VALUES(4, 501, 203, 309, 408, 100);
INSERT INTO lessons VALUES(5, 501, 202, 302, 407, 104);
INSERT INTO lessons VALUES(5, 501, 203, 309, 408, 100);

--Вторник
INSERT INTO lessons VALUES(5, 502, 202, 304, 414, 108);
INSERT INTO lessons VALUES(5, 502, 203, 304, 414, 108);

--Среда
INSERT INTO lessons VALUES(1, 503, 202, 310, 406, 110);
INSERT INTO lessons VALUES(1, 503, 203, 310, 406, 110);
INSERT INTO lessons VALUES(2, 503, 202, 311, 405, 111);
INSERT INTO lessons VALUES(2, 503, 203, 311, 405, 111);
INSERT INTO lessons VALUES(3, 503, 202, 311, 405, 111);
INSERT INTO lessons VALUES(3, 503, 203, 311, 405, 111);
INSERT INTO lessons VALUES(4, 503, 202, 310, 407, 110);
INSERT INTO lessons VALUES(4, 503, 203, 310, 407, 110);
INSERT INTO lessons VALUES(5, 503, 202, 305, 406, 112);
INSERT INTO lessons VALUES(5, 503, 203, 305, 406, 112);

--Четверг
INSERT INTO lessons VALUES(1, 504, 202, 312, 405, 103);
INSERT INTO lessons VALUES(1, 504, 203, 312, 405, 103);
INSERT INTO lessons VALUES(2, 504, 202, 313, 406, 113);
INSERT INTO lessons VALUES(2, 504, 203, 313, 406, 113);
INSERT INTO lessons VALUES(3, 504, 202, 312, 407, 103);
INSERT INTO lessons VALUES(3, 504, 203, 312, 407, 103);
INSERT INTO lessons VALUES(4, 504, 202, 313, 408, 114);
INSERT INTO lessons VALUES(4, 504, 203, 313, 408, 114);
INSERT INTO lessons VALUES(5, 504, 202, 314, 409, 115);
INSERT INTO lessons VALUES(5, 504, 203, 314, 409, 115);

--Пятница
INSERT INTO lessons VALUES(1, 505, 202, 309, 405, 100);
INSERT INTO lessons VALUES(1, 505, 203, 302, 405, 104);
INSERT INTO lessons VALUES(2, 505, 202, 309, 405, 100);
INSERT INTO lessons VALUES(2, 505, 203, 302, 405, 104);
INSERT INTO lessons VALUES(3, 505, 202, 315, 406, 116);
INSERT INTO lessons VALUES(3, 505, 203, 315, 406, 116);
INSERT INTO lessons VALUES(5, 505, 202, 304, 414, 108);
INSERT INTO lessons VALUES(5, 505, 203, 304, 414, 108);
