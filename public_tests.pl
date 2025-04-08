:- consult('37.pl').
:- begin_tests(public1).
:- use_module(library(lists)).

%%%%%%%%%%%%%%%%student_schedule_2_tests%%%%%%%%%%%%%%


test(student_schedule_2):-
    student_schedule(student_7, [slot(monday, 2, csen401), slot(sunday, 3, csen403), slot(tuesday, 4, csis402),      slot(monday, 5, de404), slot(thursday, 5, elct401), slot(wednesday, 1, math401), slot(monday, 4, rpw401)]).


test(student_schedule_3):-
    \+student_schedule(student_7, [slot(monday, 2, csen401), slot(sunday, 3, csen403), slot(tuesday, 4, csis402),slot(monday, 5, de404)]).
    


test(student_schedule_5):-
    student_schedule(student_19, [slot(sunday, 1, csen403), slot(saturday, 1, csen602), slot(thursday, 2, csen603), slot(monday, 2, csen604), slot(thursday, 4, csen907)]).
    
test(student_schedule_6):-
    \+student_schedule(student_19, [slot(sunday, 1, csen403), slot(saturday, 1, csen602), slot(thursday, 2, csen603), slot(monday, 2, csen604), slot(thursday, 4, csen1001)]).


%%%%%%%%%%%%%%%%assembly_hours_tests%%%%%%%%%%%%%%

    
test(assembly_hours_2):-
assembly_hours([sched(student_16, [slot(saturday, 1, csen601), slot(saturday, 3, csen602), slot(thursday, 2, csen603), slot(monday, 2, csen604), slot(sunday, 1, dmet604)]), sched(student_17, [slot(saturday, 1, csen601), slot(saturday, 3, csen602), slot(thursday, 2, csen603), slot(monday, 2, csen604), slot(sunday, 1, dmet604)]), sched(student_18, [slot(sunday, 1, csen403), slot(thursday, 2, csen603), slot(monday, 2, csen604), slot(tuesday, 3, dmet604), slot(sunday, 2, math401)]), sched(student_19, [slot(sunday, 1, csen403), slot(saturday, 1, csen602), slot(thursday, 2, csen603), slot(monday, 2, csen604), slot(thursday, 4, csen907)]), sched(student_26, [slot(tuesday, 2, csen1002), slot(sunday, 1, csen1003), slot(thursday, 4, csen907), slot(sunday, 2, dmet1001), slot(wednesday, 2, huma1001), slot(tuesday, 3, netw1009)]), sched(student_27, [slot(tuesday, 2, csen1002), slot(sunday, 1, csen1003), slot(thursday, 4, csen907), slot(sunday, 2, dmet1001), slot(wednesday, 2, huma1001), slot(tuesday, 3, netw1009)]), sched(student_28, [slot(tuesday, 2, csen1002), slot(sunday, 1, csen1003), slot(saturday, 1, csen602), slot(thursday, 2, csen603), slot(thursday, 4, csen907), slot(tuesday, 3, netw1009)]), sched(student_29, [slot(tuesday, 2, csen1002), slot(tuesday, 3, csen401), slot(sunday, 1, csen403), slot(thursday, 4, csen907), slot(sunday, 2, dmet1001), slot(wednesday, 2, huma1001)]), sched(student_6, [slot(monday, 2, csen401), slot(sunday, 1, csen403), slot(tuesday, 4, csis402), slot(monday, 5, de404), slot(wednesday, 3, elct401), slot(sunday, 2, math401), slot(sunday, 3, rpw401)]), sched(student_7, [slot(monday, 2, csen401), slot(sunday, 1, csen403), slot(tuesday, 4, csis402), slot(monday, 5, de404), slot(wednesday, 3, elct401), slot(sunday, 2, math401), slot(sunday, 3, rpw401)]), sched(student_8, [slot(monday, 2, csen401), slot(saturday, 1, csen602), slot(sunday, 1, csis402), slot(monday, 5, de404), slot(tuesday, 4, elct401), slot(sunday, 2, math401), slot(saturday, 3, rpw401)]), sched(student_9, [slot(monday, 2, csen401), slot(saturday, 1, csen601), slot(monday, 5, de404), slot(sunday, 2, dmet1001), slot(wednesday, 3, elct401), slot(wednesday, 1, math401), slot(saturday, 3, rpw401)])], [slot(sunday, 4), slot(sunday, 5)]).



test(assembly_hours_3):-
\+assembly_hours([sched(student_16, [slot(saturday, 1, csen601), slot(saturday, 3, csen602), slot(thursday, 2, csen603), slot(monday, 2, csen604), slot(sunday, 1, dmet604)]), sched(student_17, [slot(saturday, 1, csen601), slot(saturday, 3, csen602), slot(thursday, 2, csen603), slot(monday, 2, csen604), slot(sunday, 1, dmet604)]), sched(student_18, [slot(sunday, 1, csen403), slot(thursday, 2, csen603), slot(monday, 2, csen604), slot(tuesday, 3, dmet604), slot(sunday, 2, math401)]), sched(student_19, [slot(sunday, 1, csen403), slot(saturday, 1, csen602), slot(thursday, 2, csen603), slot(monday, 2, csen604), slot(thursday, 4, csen907)]), sched(student_26, [slot(tuesday, 2, csen1002), slot(sunday, 1, csen1003), slot(thursday, 4, csen907), slot(sunday, 2, dmet1001), slot(wednesday, 2, huma1001), slot(tuesday, 3, netw1009)]), sched(student_27, [slot(tuesday, 2, csen1002), slot(sunday, 1, csen1003), slot(thursday, 4, csen907), slot(sunday, 2, dmet1001), slot(wednesday, 2, huma1001), slot(tuesday, 3, netw1009)]), sched(student_28, [slot(tuesday, 2, csen1002), slot(sunday, 1, csen1003), slot(saturday, 1, csen602), slot(thursday, 2, csen603), slot(thursday, 4, csen907), slot(tuesday, 3, netw1009)]), sched(student_29, [slot(tuesday, 2, csen1002), slot(tuesday, 3, csen401), slot(sunday, 1, csen403), slot(thursday, 4, csen907), slot(sunday, 2, dmet1001), slot(wednesday, 2, huma1001)]), sched(student_6, [slot(monday, 2, csen401), slot(sunday, 1, csen403), slot(tuesday, 4, csis402), slot(monday, 5, de404), slot(wednesday, 3, elct401), slot(sunday, 2, math401), slot(sunday, 3, rpw401)]), sched(student_7, [slot(monday, 2, csen401), slot(sunday, 1, csen403), slot(tuesday, 4, csis402), slot(monday, 5, de404), slot(wednesday, 3, elct401), slot(sunday, 2, math401), slot(sunday, 3, rpw401)]), sched(student_8, [slot(monday, 2, csen401), slot(saturday, 1, csen602), slot(sunday, 1, csis402), slot(monday, 5, de404), slot(tuesday, 4, elct401), slot(sunday, 2, math401), slot(saturday, 3, rpw401)]), sched(student_9, [slot(monday, 2, csen401), slot(saturday, 1, csen601), slot(monday, 5, de404), slot(sunday, 2, dmet1001), slot(wednesday, 3, elct401), slot(wednesday, 1, math401), slot(saturday, 3, rpw401)])], [slot(sunday, 4), slot(monday, 2)]).



test(assembly_hours_5):-
    assembly_hours([sched(student_17, [slot(saturday, 1, csen601), slot(saturday, 3, csen602), slot(thursday, 2, csen603), slot(monday, 2, csen604), slot(sunday, 1, dmet604)]), sched(student_28, [slot(tuesday, 2, csen1002), slot(sunday, 1, csen1003), slot(saturday, 1, csen602), slot(thursday, 2, csen603), slot(thursday, 4, csen907), slot(tuesday, 3, netw1009)])], [slot(saturday, 2), slot(saturday, 4), slot(saturday, 5), slot(sunday, 2), slot(sunday, 3), slot(sunday, 4), slot(sunday, 5), slot(thursday, 1), slot(thursday, 3), slot(thursday, 5)]).

test(assembly_hours_6):-
\+assembly_hours([sched(student_17, [slot(saturday, 1, csen601), slot(saturday, 3, csen602), slot(thursday, 2, csen603), slot(monday, 2, csen604), slot(sunday, 1, dmet604)]), sched(student_28, [slot(tuesday, 2, csen1002), slot(sunday, 1, csen1003), slot(saturday, 1, csen602), slot(thursday, 2, csen603), slot(thursday, 4, csen907), slot(tuesday, 3, netw1009)])], [slot(saturday, 2), slot(saturday, 4), slot(saturday, 5), slot(sunday, 2), slot(sunday, 3), slot(sunday, 4), slot(sunday, 5), slot(thursday, 1), slot(thursday, 3), slot(thursday, 5), slot(wednesday, 1)]).



%%%%%%%%%%%%university_schedule_tests%%%%%%%%%%%%%%%

test(university_schedule_2):-
    university_schedule([sched(student_17, [slot(saturday, 1, csen601), slot(saturday, 3, csen602), slot(thursday, 2, csen603), slot(monday, 2, csen604), slot(sunday, 1, dmet604)]), sched(student_18, [slot(sunday, 1, csen403), slot(thursday, 2, csen603), slot(monday, 2, csen604), slot(tuesday, 3, dmet604), slot(sunday, 2, math401)]), sched(student_19, [slot(sunday, 1, csen403), slot(saturday, 1, csen602), slot(thursday, 2, csen603), slot(monday, 2, csen604), slot(thursday, 4, csen907)]), sched(student_27, [slot(tuesday, 2, csen1002), slot(sunday, 1, csen1003), slot(thursday, 4, csen907), slot(sunday, 2, dmet1001), slot(wednesday, 2, huma1001), slot(tuesday, 3, netw1009)]), sched(student_28, [slot(tuesday, 2, csen1002), slot(sunday, 1, csen1003), slot(saturday, 1, csen602), slot(thursday, 2, csen603), slot(thursday, 4, csen907), slot(tuesday, 3, netw1009)]), sched(student_29, [slot(tuesday, 2, csen1002), slot(tuesday, 3, csen401), slot(sunday, 1, csen403), slot(thursday, 4, csen907), slot(sunday, 2, dmet1001), slot(wednesday, 2, huma1001)]), sched(student_7, [slot(monday, 2, csen401), slot(sunday, 1, csen403), slot(tuesday, 4, csis402), slot(monday, 5, de404), slot(wednesday, 3, elct401), slot(sunday, 2, math401), slot(sunday, 3, rpw401)]), sched(student_8, [slot(monday, 2, csen401), slot(saturday, 1, csen602), slot(sunday, 1, csis402), slot(monday, 5, de404), slot(tuesday, 4, elct401), slot(sunday, 2, math401), slot(saturday, 3, rpw401)]), sched(student_9, [slot(monday, 2, csen401), slot(saturday, 1, csen601), slot(monday, 5, de404), slot(sunday, 2, dmet1001), slot(wednesday, 3, elct401), slot(wednesday, 1, math401), slot(saturday, 3, rpw401)])]).


test(university_schedule_3):-
    \+university_schedule([sched(student_0, [slot(saturday, 1, csen202), slot(sunday, 1, mech202), slot(monday, 1, physics201)]), sched(student_1, [slot(saturday, 1, physics201)]), sched(student_2, [slot(tuesday, 1, csen202), slot(saturday, 2, signls402)])] ).

test(university_schedule_4):-
    \+ university_schedule([sched(student_0,L)]).



%%%%%%%%%%%%%%%%no_clashes_tests%%%%%%%%%%%%%%
test(no_clashes_1):-
    no_clashes([slot(saturday, 1, csen202), slot(sunday, 1, mech202), slot(monday, 1, physics201)]).

test(no_clashes_2):-
    \+no_clashes([slot(sunday, 1, csen202), slot(sunday, 1, mech202), slot(monday, 1, physics201)]).

test(no_clashes_3):-
    \+no_clashes([slot(sunday, 1, csen202), slot(sunday, 1, csen202), slot(monday, 1, physics201)]).

%%%%%%%%%%%%%%%%study_days_tests%%%%%%%%%%%%%%

test(study_days_1):-
    study_days([slot(saturday, 1, csen202), slot(sunday, 1, mech202), slot(monday, 1, physics201)], 5).

test(study_days_2):-
    \+study_days([slot(saturday, 1, csen202), slot(sunday, 1, mech202), slot(monday, 1, physics201)], 2).

test(study_days_3):-
        study_days([slot(saturday, 1, csen202), slot(sunday, 1, mech202), slot(monday, 1, physics201)], 3).

:- end_tests(public1).
