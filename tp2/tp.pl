%Knowledge base for the Computer Engineering program

%Courses
course(inf1005c).
course(inf1500).
course(mth1101).
course(mth1007).
course(inf1040).
course(inf1010).
course(log1000).
course(inf1600).
course(mth1102).
course(ssh3201).
course(inf1995).
course(inf2990).
course(inf3995).
course(inf4990).

%Engineering programs
program(computer_eng).
program(software_eng).
program(biomedical_eng).
program(civil_eng).
program(industrial_eng).
program(mechanical_eng).
program(physics_eng).
program(mathematics_eng).

%Which courses are in which programs
program_courses(computer_eng, [inf1005c, inf1500, mth1101, mth1007, inf1040, inf1010, log1000, inf1600, mth1102, ssh3201]).
program_courses(biomedical_eng, [mth1101, mth1007, mth1102]).

course_in_programs(Course, Programs) :-
	findall(Program, (program_courses(Program, Courses), member(Course, Courses)), Programs).

%Credits
credits(inf1005c, 3).
credits(inf1500, 3).
credits(mth1101, 2).
credits(mth1007, 2).
credits(inf1040, 3).
credits(inf1010, 3).
credits(log1000, 3).
credits(inf1600, 3).
credits(mth1102, 2).
credits(ssh3201, 3).

%Inverted classes
inverted_class(log1000).
inverted_class(inf1005c).

inverted_classes(Classes) :-
	findall(Class, inverted_class(Class), Classes).
	
%mandatory courses
mandatory(inf1005c).
mandatory(inf1010).
mandatory(mth1101).
mandatory(mth1007).

mandatory_courses(Courses, MandatoryCourses) :-
	findall(MandatoryCourse, (member(MandatoryCourse, Courses), mandatory(MandatoryCourse)), MandatoryCourses).
	
%Optional courses
optional(log1000).
optional(ssh3201).

optional_courses(Courses, OptionalCourses) :-
	findall(OptionalCourse, (member(OptionalCourse, Courses), optional(OptionalCourse)), OptionalCourses).
	
%Project courses
project(inf1995).
project(inf2990).
project(inf3995).
project(inf4990).

project_courses(Courses, ProjectCourses) :-
	findall(ProjectCourse, (member(ProjectCourse, Courses), project(ProjectCourse)), ProjectCourses).

courses_types(Courses, MandatoryCourses, OptionalCourses, ProjectCourses) :-
	valid_courses(Courses),
	mandatory_courses(Courses, MandatoryCourses),
	optional_courses(Courses, OptionalCourses),
	project_courses(Courses, ProjectCourses).
	
%prerequisites
prerequesite(inf1600, [inf1005c, inf1500]).
prerequisite(mth1102, [mth1101]).

%corequisites
corequisite(mth1102, [mth1007]).

%Credits requirements
credits_needed(ssh3201, 6).

%Programming languages in each course
use_programming_languages(inf1005c, [c]).
use_programming_languages(inf1010, [cpp]).
use_programming_languages(inf1600, [c, asm]).

courses_with_programming_language(Courses, Language) :-
	findall(Course, (use_programming_languages(Course, Languages), member(Language, Languages)), Courses).

%validators
valid_choice(Chosen, Completed) :-
	no_duplicates(Chosen),
	no_duplicates(Completed),
	valid_courses(Chosen),
	satisfies_prerequisites(Chosen, Completed),
	satisfies_corequisites(Chosen, Completed),
	satisfies_credits_req(Chosen, Completed).

no_duplicates([]).
no_duplicates([H | T]) :-
	member(H, T),
	!,
	fail.
no_duplicates([_ | T]) :-
	no_duplicates(T).
	
valid_courses([]).
valid_courses([Course | Courses]) :-
	course(Course),
	valid_courses(Courses).
	
satisfies_prerequisites([], _).
satisfies_prerequisites([ChosenCourse | ChosenCourses], Completed) :-
	(prerequesite(ChosenCourse, X) -> Prerequisites = X ; Prerequisites = []),
	subset(Prerequisites, Completed),
	satisfies_prerequisites(ChosenCourses, Completed).
	
satisfies_corequisites([], _).
satisfies_corequisites([ChosenCourse | ChosenCourses], Completed) :-
	append([ChosenCourse | ChosenCourses], Completed, AllCourses),
	(corequisite(ChosenCourse, X) -> Corequisites = X ; Corequisites = []),
	subset(Corequisites, AllCourses),
	satisfies_corequisites(ChosenCourses, Completed).
	
satisfies_credits_req([], _).
satisfies_credits_req(ChosenCourses, Completed) :-
	credit_count(Completed, Credits),
	forall(member(Course, ChosenCourses), ((credits_needed(Course, X) -> Needed = X ; Needed = 0), Credits >= Needed)).

credit_count([], 0).
credit_count([Course | Courses], Credits) :-
	credit_count(Courses, Sum),
	credits(Course, Count),
	Credits is Sum + Count.
	
%Quality out of 10
choice_quality(Courses, Completed, Points) :-
	credit_count(Courses, Credits),
	(Credits < 3 -> CreditPoints = 0 ; (Credits < 12 -> CreditPoints = 2 ; CreditPoints = 5)), %  3 < Part-time < 12 and Full-time >= 12 
	(valid_choice(Courses, Completed) -> ValidPoints = 5 ; ValidPoints = 0),
	Points is CreditPoints + ValidPoints.
	

%Subject hierarchy
subject(mathematics).
subject(physics).
subject(programming).
subject(procedural_programming).
subject(oob_programming).
subject(embedded).

direct(programming, mathematics).
direct(procedural_programming, programming).
direct(oob_programming, programming).

has_subjects(inf1005c, [procedural_programming]).
has_subjects(inf1600, [embedded, procedural_programming]).
has_subjects(inf1010, [oob_programming]).

is_subject(X, X).
is_subject(X, Y) :- 
	subject(X), 
	subject(Y), 
	direct(X, Y).	
is_subject(X, Y) :- 
	subject(X), 
	subject(Y), 
	subject(Z), 
	direct(X, Z), 
	is_subject(Z, Y).
	
courses_with_subject(Subject, Courses) :-
	setof(ChildSubject, is_subject(ChildSubject, Subject), Subjects),
	findall(Course, (has_subjects(Course, CourseSubjects), intersection(Subjects, CourseSubjects, Int), list_length(Int, Length), Length > 0) , Courses).

	
%helpers
subset([], _).
subset([H|T], L) :-
	member(H, L),
	subset(T, L).
	
is_empty([]).

list_length([], 0).
list_length([_ | T], L) :-
	length(T, L2),
	L is L2 + 1.