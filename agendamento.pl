:- use_module(library(clpfd)).
:- use_module(library(lists)).



% tipos de participantes
% 1 - presenca obrigatoria
% 2 - presenca obrigatoria + distancia
% 3 - presenca preferencial
% 4 - presenca preferencial + distancia
% 5 - presenca opcional

% tipo de sala
% 1 - normal
% 2 - c/ videoconferencia


%Result = [Idsala1(tam4),Id1,Id2,Id3,0,Idsala2,Id4,Id5,...]

%%%%%%%%%%%%% AUX %%%%%%%%%%%%%%%%%
flatten(List, FlatList) :-
 flatten(List, [], FlatList0), !,
 FlatList = FlatList0.

 flatten(Var, Tl, [Var|Tl]) :- var(Var), !.
 flatten([], Tl, Tl) :- !.
 flatten([Hd|Tl], Tail, List) :- !,
     flatten(Hd, FlatHeadTail, List),
     flatten(Tl, Tail, FlatHeadTail).
flatten(NonList, Tl, [NonList|Tl]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reuniao1([15,[1,2,3],[4,5,6],[7],[]]).
reuniao2([10,[1,2,3],[4,5,6],[],[]]).
reuniao3([11,[1,2,3],[4,5,6],[7],[]]).
reuniao4([12,[1,2,3],[4,5,6],[],[]]).

reuniaoteste1([1,2,3],[1,4,5]).
reuniaoprefteste1([1,2,3],[3,5,5]).
%sala[id,capacidade,tipo].
salasteste1([10,10,20,10,10,20]-[1,1,2,1,1,2]).


getMaxValue(R_ids,Value) :- 
	maximum(Value,R_ids).

getTotalSize([],0).
getTotalSize([Sala1|T],Size) :- 
	getTotalSize(T,TSize),
	element(2,Sala1,X),
	X1 #= X + 1,
	Size #= X1 + TSize.
	
createTasks([],_,_,_,[]).
createTasks([Hss|Tss],[Hd|Td],[He|Te],[Hm|Tm],L) :-
	createTasks(Tss,Td,Te,Tm,Ltemp),
	append([task(Hss,Hd,He,1,Hm)],Ltemp,L).

createMachines([],[],_).	
createMachines([_Hsala|Tsala],L,Count) :-
	Count1 is Count + 1,
	createMachines(Tsala,Ltemp,Count1),
	append([machine(Count,1)],Ltemp,L).
	
teste :-
	reuniao1(A),
	reuniao2(B),
	reuniao3(C),
	reuniao4(D),
	L = [A,B,C,D],
	salasteste1(S),
	agendamento(L,S).	

teste2(V) :- 
	length(T,3),
	domain(T,1,2),
	L = [1,2,4,5],
	element(V,L,2),
	labeling([],T).
	
	
agendamento(ReunioesPretendidas,SalasCap-SalasCar) :- 
	length(ReunioesPretendidas,Rsize),
	length(SS,Rsize),
	length(D,Rsize),
	length(ES,Rsize),
	length(SalasReuniao,Rsize),
	length(Reunioes,Rsize),
	createTasks(SS,D,ES,SalasReuniao,ReunioesTask),
	createMachines(SalasCap,Machines,1),
	
	
	domain(SS,1,100),
	domain(D,1,100),
	domain(ES,1,100),
	domain(SalasReuniao,1,Rsize),
	domain([End],1,100),
	maximum(End,ES),
	
	assign_salas_reuniao(SalasReuniao,ReunioesPretendidas,SalasCap-SalasCar),
	assign_reuniao_participantes(Reunioes,ReunioesPretendidas,SalasReuniao,SalasCap),
	%assign_stuff(SS,D,ES,ReunioesPretendidas),
	cumulatives(ReunioesTask,Machines,[bound(upper)]),
	
	
	
	append(SS,[End],Vars1),
	append(Vars1,SalasReuniao,Vars),
	labeling([],Vars),
	write(SS),nl,write(End),nl,write(SalasReuniao).
	
	
/*assign_stuff([HSS|TSS],[HD|TD],[HES|TES],[HReunioesPretendidas|TReunioesPretendidas]).*/
	
assign_salas_reuniao(_,[],_).
assign_salas_reuniao([HSalasReuniao|TSalasReuniao],[ [_,MO,_MP,MOD,_MN] |TReunioesPretendidas],SalasCap-SalasCar) :- 
	element(I,SalasCar,V),
	element(I,SalasCap,Cap),
	length(MOD,X),
	length(MO,Y),
	Cap in 0..100,
	Y - 1 #< Cap,
	(V #= 2 #/\ X #> 0) #\/ X #= 0,
	HSalasReuniao #= I,
	assign_salas_reuniao(TSalasReuniao,TReunioesPretendidas,SalasCap-SalasCar).


force_obrigatorios(_HReunioes,[],[]).
force_obrigatorios(HReunioes,[],[HMOD|TMOD]) :- 
	element(_I,HReunioes,HMOD), %força que user HMOD exista na reuniao
	force_obrigatorios(HReunioes,[],TMOD).
force_obrigatorios(HReunioes,[HMO|TMO],MOD) :- 
	element(_I,HReunioes,HMO), %força que user HMO exista na reuniao
	force_obrigatorios(HReunioes,TMO,MOD).
	
assign_reuniao_participantes(_,[],[],_).	
assign_reuniao_participantes([HReunioes|TReunioes],[ [_,MO,_MP,MOD,_MN] |TReunioesPretendidas],[HSR|TSR],SalasCap) :- 
	length(HReunioes,NumParticipantes),
	length(MO,NumObg),
	length(MOD,NumObgDist),
	element(1,HReunioes,HSR), %coloca id sala na primeira posiçao
	force_obrigatorios(HReunioes,MO,MOD),
	NumParticipantes #> NumObg + NumObgDist,
	element(HSR,SalasCap,Cap),
	NumParticipantes - NumObgDist - 1 #< Cap + 1,
	assign_reuniao_participantes(TReunioes,TReunioesPretendidas,TSR,SalasCap).
	
	
	
/*avoid_same_time(SS,ES,Result) :- */
	
        
        
        
        