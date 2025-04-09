:- consult('studentKB').
:- consult('publicKB').

find_slot(_,[],6,_):- fail.
find_slot(Course, [SlotCourses | _], SlotNumber, SlotNumber) :-
    member(Course, SlotCourses). 
find_slot(Course, [SlotCourses | Rest], N, SlotNumber) :-
	N1 is N + 1,                 
    find_slot(Course, Rest, N1, SlotNumber).
	
assign_slots([], []).
assign_slots([Course|RestCourses], [slot(Day, SlotNumber, Course)|RestSlots]) :-
    day_schedule(Day, Schedule),
    find_slot(Course, Schedule, 1, SlotNumber),
    assign_slots(RestCourses, RestSlots).

student_schedule(Student, Slots) :-
    setof(Course, studies(Student, Course), Courses),
    assign_slots(Courses, Slots),
    no_clashes(Slots),
    study_days(Slots, 5).	

/*
get_all_for_slot(_,[],_,[]).
get_all_for_slot(Day, [H|T], Cnt, Res):-
	get_all_for_slot(Day,T,Cnt,R1),
	append([slot(Day, Cnt, H)], R1, Res). 
	
	
get_slots_for_day(day_schedule(Day,[]), _ , []).
get_slots_for_day(day_schedule(Day,[H|T]),Cnt, Res):-
	get_all_for_slot(Day, H, Cnt, R1),
	Cnt2 is Cnt + 1,
	get_slots_for_day(day_schedule(Day,T),Cnt2, R2),
	append(R1, R2, Res).

get_slots([],[]).	
get_slots([H|T], All_slots):-
	get_slots_for_day(H,1,S1),
	get_slots(T,S2),
	append(S1, S2, All_slots).
	
day_slots(Days_sched):-
	findall(day_schedule(D,L),day_schedule(D,L),Days_sched).
	
ss_helper2([],_,_,[]).	

ss_helper2([slot(D,N,S)|T], Student, Acc, Slots):-
	studies(Student, S), \+ member(slot(_,_,S), Acc),
	ss_helper2(T, Student, [slot(D,N,S)|Acc] , S2),
	append([slot(D,N,S)], S2, Slots).

ss_helper2([slot(D,N,S)|T], Student, Acc, Slots):-
	studies(Student, S), \+ member(slot(_,_,S), Acc),
	ss_helper2(T, Student, Acc , Slots).
	
ss_helper2([slot(D,N,S)|T], Student, Acc, Slots):-
	studies(Student, S), member(slot(_,_,S), Acc),
	ss_helper2(T, Student, Acc, Slots).
	
ss_helper2([slot(D,N,S)|T], Student, Acc, Slots):-
	\+studies(Student, S),
	ss_helper2(T, Student, Acc , Slots).
		
studyAll(_,[],_).		
studyAll(Student, [studies(_,S)|T], Slots):-
		member(slot(_,_,S), Slots),
		studyAll(Student, T, Slots).

student_schedule_helper(Student,Slots):-
	day_slots(Days_sched), get_slots(Days_sched, All),
	ss_helper2(All, Student, [], Slots),
	findall(studies(Student,Sub), studies(Student,Sub), Sub),
	studyAll(Student, Sub, Slots),
	no_clashes(Slots),     
    study_days(Slots, 5).
	
	
student_schedule(Student, Slots) :-
		student_schedule_helper(Student, Slots).
*/	

uni_helper([],[]).
uni_helper([H|T], [sched(H,H1)|T1]):-
	student_schedule(H, H1),
	uni_helper(T,T1).

university_schedule(S) :-
     % Adding Course^ tells Prolog to collect unique Student values only, ignoring Course.
     setof(Student, Course^studies(Student, Course), Students), 
	uni_helper(Students, S).
	 
	 
	 /*setof(sched(Student, Slots), 
           (member(Student, Students), student_schedule(Student, Slots)), 
           S).
 */
assembly_hours(Schedules, AH) :-
    findall(slot(Day, SlotNumber), (
        day_schedule(Day, _),          
        between(1, 5, SlotNumber),     
        forall(
            member(sched(_, StudentSlots), Schedules), 
            ( 
                \+ member(slot(Day, SlotNumber, _), StudentSlots), 
                member(slot(Day, _, _), StudentSlots)               
            )
        )
    ), AH).
	
	
count_days([],_,0).
count_days([slot(X,_,_)|T],Acc,N):-
	\+ member(X,Acc),
	count_days(T,[X|Acc],N1),
	N is N1+1.
count_days([slot(X,_,_)|T],Acc,N):-
	member(X,Acc),
	count_days(T,Acc,N).
study_days(Slots,DayCount):-
	count_days(Slots,[],N),
	N=<DayCount.
	
no_clashes([],_).
no_clashes([slot(X,Y,_)|T],Acc):-
	\+ member(pair(X,Y),Acc),
	no_clashes(T,[pair(X,Y)|Acc]).
no_clashes(Slots):-
	no_clashes(Slots,[]).