studies(student_0, physics201).
studies(student_0, mech202).
studies(student_0, csen202).

studies(student_1, physics201).
studies(student_1, physics201).

studies(student_2, csen202).
studies(student_2, signls402).

studies(student_3, netw201).
studies(student_3, elct402).
studies(student_3, signals402).

day_schedule(saturday, [[math203],[physics201, csen202],[signls402, netw201],[],[]]).
day_schedule(sunday, [[math203],[chem101, mech202],[],[],[]]).
day_schedule(monday, [[math203],[physics201, csen202],[],[],[]]).
day_schedule(tuesday, [[signals402],[physics201, csen202],[],[],[]]).
day_schedule(wednesday, [[math203],[],[physics201, elct402],[],[]]).
day_schedule(thursday, [[math203],[],[],[netw201, csen202],[]]).


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
    setof(Student, Course^studies(Student, Course), Students), 
    setof(sched(Student, Slots), 
          (member(Student, Students), student_schedule(Student, Slots)), S).
		  
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