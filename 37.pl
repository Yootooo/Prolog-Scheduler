:- consult('studentKB').


find_slot(Course, [SlotCourses | _], SlotNumber, SlotNumber) :-
    member(Course, SlotCourses), !. 
find_slot(Course, [_ | Rest], N, SlotNumber) :-
    N1 is N + 1,                 
    find_slot(Course, Rest, N1, SlotNumber).
	
student_schedule(Student, Slots) :-
    setof(slot(Day, SlotNumber, Course), 
          (studies(Student, Course), 
           day_schedule(Day, Schedule), 
           find_slot(Course, Schedule, 1, SlotNumber)), 
          Slots),
		  
    no_clashes(Slots),     
    study_days(Slots, 5).  
	

% Find all valid (Day, Slot) pairs where all students are free 
assembly_hours(Student_schedule, AH) :-
    setof(slot(Day, Slot),
            ( all_students_attend(Student_schedule, Day),
              between(0, 5, Slot),
              all_students_available(Student_schedule, Slot)
            ),
            AH).

% Check if all students attend on a given day.
all_students_attend([], _).
all_students_attend([sched(_, Student_schedule)|T], Day_to_check):-
    member(slot(Day_to_check, _, _), Student_schedule),
    all_students_attend(T, Day_to_check).

% Check if all students are available at a given slot.
all_students_available([], _).
all_students_available([sched(_, Student_schedule)|T], Slot_to_check) :-
    \+ member(slot(_, Slot_to_check, _), Student_schedule),
    all_students_available(T, Slot_to_check).



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