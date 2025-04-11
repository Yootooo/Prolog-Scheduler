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


uni_helper([],[]).
uni_helper([H|T], [sched(H,H1)|T1]):-
	student_schedule(H, H1),
	uni_helper(T,T1).

university_schedule(S) :-
    setof(Student, Course^studies(Student,Course), Students),
	uni_helper(Students, S).

	 
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
        )), AH).
	
	
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
	