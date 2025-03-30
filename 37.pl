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
	
university_schedule(S) :-
    % Adding Course^ tells Prolog to collect unique Student values only, ignoring Course.
    setof(Student, Course^studies(Student, Course), Students), 
    setof(sched(Student, Slots), 
          (member(Student, Students), student_schedule(Student, Slots)), 
          S).


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
