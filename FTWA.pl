% Yardımcı predikatlar
:- dynamic person/8.
:- dynamic marriage/2.
:- dynamic printed/1.
:- dynamic level_cache/2.

% person(Gender, FullName, Surname, BirthYear, DeathYear, FatherFullName, MotherFullName, ChildFullName).

% Başlangıçta kayıtlı iki kişi (rootlar)
person(male, 'Murat Aslan', 'Aslan', 1940, 0, '', '', '').
person(female, 'Sedanur Aslan', 'Aslan', 1942, 0, '', '', '').

current_year(2025).

% Full name oluşturma: iki atomu string olarak birleştirir
full_name(Name, Surname, FullName) :-
    atomic_list_concat([Name, Surname], ' ', FullName).

% Kişinin yaşı
age(FullName, Age) :-
    person(_, FullName, _, BirthYear, DeathYear, _, _, _),
    current_year(CY),
    ( DeathYear =:= 0 -> Age is CY - BirthYear ; Age is DeathYear - BirthYear ).

alive(FullName) :-
    person(_, FullName, _, _, DeathYear, _, _, _),
    DeathYear =:= 0.

dead(FullName) :-
    person(_, FullName, _, _, DeathYear, _, _, _),
    DeathYear \= 0.

% Kök kişiler - sadece Murat Aslan ve Sedanur Aslan (Level 0)
level(Name, 0) :-
    member(Name, ['Murat Aslan', 'Sedanur Aslan']).

% Kan bağı olan çocuklar için level hesaplama - ÖNCELİK!
level(Name, Level) :-
    % Kök kişiler değilse devam et
    \+ member(Name, ['Murat Aslan', 'Sedanur Aslan']),
    % Kan bağı var mı kontrol et (ebeveynlerden en az biri var)
    person(_, Name, _, _, _, Father, Mother, _),
    (Father \= none, Father \= ''; Mother \= none, Mother \= ''),
    % Baba tarafından
    (Father \= none, Father \= '' ->
        level(Father, L0),
        Level is L0 + 1
    ;
    % Anne tarafından  
    Mother \= none, Mother \= '' ->
        level(Mother, L0),
        Level is L0 + 1
    ),
    !. % Kan bağı bulunduğunda dur, evlilik kuralını çalıştırma

% Evlilik yoluyla bağlantılı kişiler için (damat-gelin)
% SADECE kan bağı OLMAYAN kişiler için - BASİT VERSİYON
level(Name, Level) :-
    % Kök kişiler değilse
    \+ member(Name, ['Murat Aslan', 'Sedanur Aslan']),
    % Bu kişinin kan bağı KESINLIKLE yoksa (ebeveynleri none veya boş)
    person(_, Name, _, _, _, Father, Mother, _),
    (Father = none; Father = ''),
    (Mother = none; Mother = ''),
    % Eşini bul (aile üyesi olan kişi)
    find_spouse(Name, FamilyMemberSpouse),
    FamilyMemberSpouse \= '',
    FamilyMemberSpouse \= none,
    % Aile üyesi olan eşin level'ını bul ve aynı level'a yerleştir
    level(FamilyMemberSpouse, Level),
    !.

% Çocuk sayısı
child_count(FullName, Count) :-
    findall(C, (person(_, C, _, _, _, Father, Mother, _), (Father = FullName ; Mother = FullName)), Children),
    length(Children, Count).

% Cinsiyet yardımcı predikatları
male(P)   :- person(male, P, _, _, _, _, _, _).
female(P) :- person(female, P, _, _, _, _, _, _).

mother(M, C) :- person(_, C, _, _, _, _, M, _).
father(F, C) :- person(_, C, _, _, _, F, _, _).

siblings(A, B) :-
    person(_, A, _, _, _, FA, MA, _),
    person(_, B, _, _, _, FB, MB, _),
    A \= B,
    FA \= '', MA \= '',
    FA = FB, MA = MB.

same_parents(P1, P2) :-
    person(_, P1, _, _, _, F1, M1, _),
    person(_, P2, _, _, _, F2, M2, _),
    F1 \= '', M1 \= '',
    F1 == F2,
    M1 == M2,
    P1 \= P2.
	
parent(Parent, Child) :-
    person(_, Child, _, _, _, Parent, _, _);
    person(_, Child, _, _, _, _, Parent, _).


% İlişki bulma
find_relation(N1, N2, 'Anne') :-
    person(female, N1, _, _, _, _, _, _),
    person(_, N2, _, _, _, _, N1, _).

find_relation(N1, N2, 'Baba') :-
    person(male, N1, _, _, _, _, _, _),
    person(_, N2, _, _, _, N1, _, _).

find_relation(N1, N2, 'Ogul') :-
    person(male, N1, _, _, _, _, _, _),
    (person(_, N1, _, _, _, N2, _, _) ;
     person(_, N1, _, _, _, _, N2, _)).

find_relation(N1, N2, 'Kiz') :-
    person(female, N1, _, _, _, _, _, _),
    (person(_, N1, _, _, _, N2, _, _) ;
     person(_, N1, _, _, _, _, N2, _)).

find_relation(N1, N2, 'Erkek Kardes') :-
    person(male, N1, _, BY1, _, _, _, _),
    person(_, N2, _, BY2, _, _, _, _),
    same_parents(N1, N2),
    BY1 > BY2,
    N1 \= N2.

find_relation(N1, N2, 'Kiz Kardes') :-
    person(female, N1, _, BY1, _, _, _, _),
    person(_, N2, _, BY2, _, _, _, _),
    same_parents(N1, N2),
    BY1 > BY2,
    N1 \= N2.

% Yaş karşılaştırmalı kardeş ilişkileri
find_relation(N1, N2, 'Abla') :-
    person(female, N1, _, BY1, _, _, _, _),
    person(_, N2, _, BY2, _, _, _, _),
    same_parents(N1, N2),
    BY1 < BY2,  % N1 daha büyük
    N1 \= N2.

find_relation(N1, N2, 'Abi') :-
    person(male, N1, _, BY1, _, _, _, _),
    person(_, N2, _, BY2, _, _, _, _),
    same_parents(N1, N2),
    BY1 < BY2,  % N1 daha büyük
    N1 \= N2.

% Amca (babanın erkek kardeşi)
find_relation(N1, N2, 'Amca') :-
    person(_, N2, _, _, _, FN, _, _),  % N2'nin babası
    person(male, N1, _, _, _, _, _, _),           % N1 erkek
    same_parents(N1, FN),              % N1 ve baba kardeş
    N1 \= FN.

% Hala (babanın kız kardeşi)
find_relation(N1, N2, 'Hala') :-
    person(_, N2, _, _, _, FN, _, _),  % N2'nin babası
    person(female, N1, _, _, _, _, _, _),         % N1 kadın
    same_parents(N1, FN),              % N1 ve baba kardeş
    N1 \= FN.

% Dayı (annenin erkek kardeşi)
find_relation(N1, N2, 'Dayi') :-
    person(_, N2, _, _, _, _, MN, _),  % N2'nin annesi
    person(male, N1, _, _, _, _, _, _),           % N1 erkek
    same_parents(N1, MN),              % N1 ve anne kardeş
    N1 \= MN.

% Teyze (annenin kız kardeşi)
find_relation(N1, N2, 'Teyze') :-
    person(_, N2, _, _, _, _, MN, _),  % N2'nin annesi
    person(female, N1, _, _, _, _, _, _),         % N1 kadın
    same_parents(N1, MN),              % N1 ve anne kardeş
    N1 \= MN.

find_relation(N1, N2, 'Yegen') :-
    same_parents(N2, Sibling),  % N2'nin kardeşi
    (person(_, N1, _, _, _, Sibling, _, _) ;  % Sibling, N1'in babası
     person(_, N1, _, _, _, _, Sibling, _)).  % veya annesi

find_relation(N1, N2, 'Kuzen') :-
    (person(_, N1, _, _, _, F1, _, _) ; person(_, N1, _, _, _, _, F1, _)),
    (person(_, N2, _, _, _, F2, _, _) ; person(_, N2, _, _, _, _, F2, _)),
    same_parents(F1, F2),
    N1 \= N2.

% Enişte (kız kardeşin kocası)
find_relation(N1, N2, 'Eniste') :-
    person(male, N1, _, _, _, _, _, _),
    person(_, N2, _, _, _, _, _, _),
    person(female, SN, _, _, _, _, _, _),  % N2'nin kız kardeşi
    same_parents(N2, SN),
    (marriage(N1, SN) ; marriage(SN, N1)).

% Yenge (erkek kardeşin karısı)
find_relation(N1, N2, 'Yenge') :-
    person(female, N1, _, _, _, _, _, _),
    person(_, N2, _, _, _, _, _, _),
    person(male, BN, _, _, _, _, _, _),    % N2'nin erkek kardeşi
    same_parents(N2, BN),
    (marriage(N1, BN) ; marriage(BN, N1)).

find_relation(N1, N2, 'Kayinvalide') :-
    person(_, N2, _, _, _, _, _, _),  % N2 kişi
    (marriage(N2, Spouse) ; marriage(Spouse, N2)),
    person(female, N1, _, _, _, _, _, _),
    person(_, Spouse, _, _, _, _, N1, _).  % N1, Spouse’un annesi

find_relation(N1, N2, 'Kayinpeder') :-
    person(_, N2, _, _, _, _, _, _),
    (marriage(N2, Spouse) ; marriage(Spouse, N2)),
    person(male, N1, _, _, _, _, _, _),
    person(_, Spouse, _, _, _, N1, _, _).

% Gelin (oğlun karısı)
find_relation(N1, N2, 'Gelin') :-
    person(female, N1, _, _, _, _, _, _),
    person(male, SN, _, _, _, _, _, _),           % N1'in kocası
    (person(_, SN, _, _, _, N2, _, _) ; % N2, SN'nin babası
     person(_, SN, _, _, _, _, N2, _)), % N2, SN'nin annesi
    (marriage(N1, SN) ; marriage(SN, N1)).

% Damat (kızın kocası)
find_relation(N1, N2, 'Damat') :-
    person(male, N1, _, _, _, _, _, _),
    person(female, SN, _, _, _, _, _, _),         % N1'in karısı
    (person(_, SN, _, _, _, N2, _, _) ; % N2, SN'nin babası
     person(_, SN, _, _, _, _, N2, _)), % N2, SN'nin annesi
    (marriage(N1, SN) ; marriage(SN, N1)).

% Bacanak (karının erkek kardeşi)
find_relation(N1, N2, 'Bacanak') :-
    person(male, N1, _, _, _, _, _, _),
    person(female, WN, _, _, _, _, _, _),         % N1'in karısı
    person(male, N2, _, _, _, _, _, _),
    same_parents(WN, N2),              % Karı ve N2 kardeş
    (marriage(N1, WN) ; marriage(WN, N1)).

% Baldız (karının kız kardeşi)
find_relation(N1, N2, 'Baldiz') :-
    person(male, N1, _, _, _, _, _, _),
    person(female, WN, _, _, _, _, _, _),         % N1'in karısı
    person(female, N2, _, _, _, _, _, _),
    same_parents(WN, N2),              % Karı ve N2 kardeş
    (marriage(N1, WN) ; marriage(WN, N1)).

% Elti (kocanın erkek kardeşinin karısı)
find_relation(N1, N2, 'Elti') :-
    person(female, N1, _, _, _, _, _, _),
    person(male, HN, _, _, _, _, _, _),          % N1'in kocası
    person(male, BN, _, _, _, _, _, _),          % Kocanın erkek kardeşi
    same_parents(HN, BN),
    person(female, N2, _, _, _, _, _, _),
    (marriage(N1, HN) ; marriage(HN, N1)),
    (marriage(N2, BN) ; marriage(BN, N2)).

% Kayınbirader (eşin erkek kardeşi)
find_relation(N1, N2, 'Kayinbirader') :-
    person(male, N1, _, _, _, _, _, _),
    person(_, SN, _, _, _, _, _, _),             % N1'in eşi
    same_parents(SN, N2),             % Eş ve N2 kardeş
    person(male, N2, _, _, _, _, _, _),
    (marriage(N1, SN) ; marriage(SN, N1)).

validate_marriage(N1, N2, G1, G2, B1, B2) :-
    G1 \= G2,
    current_year(CY),
    Age1 is CY - B1,
    Age2 is CY - B2,
    Age1 >= 18, Age2 >= 18,
    \+ close_relative(N1, N2),
    \+ forbidden_marriage(N1, N2).

% Yakın akraba kontrolü - evlilik engelleri
close_relative(P1, P2) :-
    % 1. Aynı kişi kontrolü
    P1 = P2, !.

close_relative(P1, P2) :-
    % 2. Ebeveyn-çocuk ilişkisi
    (parent(P1, P2) ; parent(P2, P1)), !.

close_relative(P1, P2) :-
    % 3. Kardeşlik (aynı ebeveynler)
    same_parents(P1, P2), !.

close_relative(P1, P2) :-
    % 4. Büyükbababüyükanne - torun ilişkisi
    (grandparent(P1, P2) ; grandparent(P2, P1)), !.

close_relative(P1, P2) :-
    % 5. Amca, dayı, hala, teyze - yeğen ilişkisi
    (uncle_aunt(P1, P2) ; uncle_aunt(P2, P1)), !.
	
close_relative(P1, P2) :-
    member(Rel, ['Yenge', 'Enişte', 'Gelin', 'Damat', 'Elti', 'Bacanak', 
                 'Kayınvalide', 'Kayınpeder', 'Baldız', 'Kayınbirader']),
    (find_relation(P1, P2, Rel) ; find_relation(P2, P1, Rel)), !.
	
% Büyükanne-büyükbaba kontrolü
grandparent(GP, GC) :-
    parent(GP, P),
    parent(P, GC),
    GP \= P, P \= GC, GP \= GC.

% Amca/dayı/hala/teyze kontrolü
uncle_aunt(UA, Nephew) :-
    % UA'nın kardeşi Nephew'nun ebeveyni
    parent(Parent, Nephew),
    same_parents(UA, Parent),
    UA \= Parent.

% Kuzen kontrolü (amca/dayı/hala/teyze çocukları)
cousins(C1, C2) :-
    parent(P1, C1),
    parent(P2, C2),
    same_parents(P1, P2),
    P1 \= P2, C1 \= C2.

% Evlilik ekleme (tam versiyon)
add_marriage :-
    % 1. Aile üyesinin bilgilerini al
    write('Type family member name and surname: '), flush_output,
    read_line_to_string(user_input, FamilyInput),
    split_string(FamilyInput, " ", "", FamilyParts),
    ( FamilyParts = [FN, FS|_] ->
        atom_string(FamilyName, FN),
        atom_string(FamilySurname, FS),
        full_name(FamilyName, FamilySurname, FamilyFullName)
    ; write('Invalid input!'), nl, fail ),

    % 2. Aile üyesinin kayıtlı olduğunu kontrol et
    ( person(FamilyGender, FamilyFullName, _, FamilyBirth, FamilyDeath, _, _, _) ->
        true
    ; write('Family member not found!'), nl, fail ),

    % 3. Evlenecek yeni kişinin bilgilerini al
    write('Type new person name and surname to marry: '), flush_output,
    read_line_to_string(user_input, NewInput),
    split_string(NewInput, " ", "", NewParts),
    ( NewParts = [NN, NS|_] ->
        atom_string(NewName, NN),
        atom_string(NewSurname, NS),
        full_name(NewName, NewSurname, NewFullName)
    ; write('Invalid input!'), nl, fail ),

    % 4. Yeni kişinin diğer bilgilerini al
    write('Type new person birth year: '), flush_output,
    read_line_to_string(user_input, BirthYearStr),
    ( number_string(BirthYear, BirthYearStr) -> true ; write('Invalid year!'), nl, fail ),

    write('Type new person death year (0 if alive): '), flush_output,
    read_line_to_string(user_input, DeathYearStr),
    ( number_string(DeathYear, DeathYearStr) -> true ; write('Invalid year!'), nl, fail ),

    write('Type new person gender (m/f): '), flush_output,
    read_line_to_string(user_input, GenderStr),
    ( (GenderStr = "m"; GenderStr = "f") -> true ; write('Invalid gender!'), nl, fail ),
    ( GenderStr = "m" -> NewGender = male ; NewGender = female ),

    % 5. Evlilik için temel kuralları kontrol et (yaş, cinsiyet)
    current_year(CurrentYear),
    FamilyAge is CurrentYear - FamilyBirth,
    NewAge is CurrentYear - BirthYear,
    ( FamilyAge < 18 -> write('Family member is underage!'), nl, fail ; true ),
    ( NewAge < 18 -> write('New person is underage!'), nl, fail ; true ),
    ( FamilyGender = NewGender -> write('Same gender marriage not allowed!'), nl, fail ; true ),

    % 6. AKRABA KONTROLÜ (EVLİLİK ENGELLERİ) - BU KISIM KRİTİK
    ( \+ close_relative(FamilyFullName, NewFullName) ->
    % Evlilik kaydını yap
    assertz(person(NewGender, NewFullName, NewSurname, BirthYear, DeathYear, '', '', '')),
    assertz(marriage(FamilyFullName, NewFullName)),
    write('Marriage added successfully.'), nl
;
    % Burada forbidden_marriage/2 çağrılır, mesaj yazdırır
    ( forbidden_marriage(FamilyFullName, NewFullName) ->
        true  % forbidden_marriage zaten mesaj ve fail içeriyor
    ; write('Invalid marriage: They are close relatives!'), nl )
).
% Kişilerin kayıtlı olduğunu kontrol et
    ( person(G1, FullName1, _, B1, _, _, _, _),
      person(G2, FullName2, _, B2, _, _, _, _) ->
        % Evlilik zaten kayıtlı mı?
        ( marriage(FullName1, FullName2) ; marriage(FullName2, FullName1) ->
            write('Marriage already exists.'), nl
        ; % Geçerli mi kontrol et
          ( validate_marriage(FullName1, FullName2, G1, G2, B1, B2) ->
                assertz(marriage(FullName1, FullName2)),
                write('Marriage added successfully.'), nl
            ; write('Invalid marriage: Close relative or underage!'), nl )
        )
    ; write('One or both persons not found.'), nl ).
	already_married(P) :-
    marriage(P, _);
    marriage(_, P).

	are_married(P1, P2) :-
    marriage(P1, P2);
    marriage(P2, P1).

	validate_and_add_marriage(P1, P2) :-
    ( forbidden_marriage(P1, P2) -> !, fail ; true ),
    (
        are_married(P1, P2)
        -> write('They are already married.'), nl
        ; assertz(marriage(P1, P2)),
          write('Marriage successfully added between '),
          write(P1), write(' and '), write(P2), write('.'), nl
    ).

% Evlilik ekleme fonksiyonu - Geliştirilmiş versiyon
add_marriage_improved :-
    write('Type family member name and surname: '), flush_output,
    read_line_to_string(user_input, Input1),
    split_string(Input1, " ", "", Parts1),
    ( Parts1 = [N1, S1|_] ->
        atom_string(Name1, N1),
        atom_string(Surname1, S1),
        full_name(Name1, Surname1, FullName1)
    ; write('Invalid input!'), nl, fail ),

    person(_, FullName1, _, _, _, _, _, _)
    -> true ; write('Family member not found!'), nl, fail,

    write('Type person name and surname to marry: '), flush_output,
    read_line_to_string(user_input, Input2),
    split_string(Input2, " ", "", Parts2),
    ( Parts2 = [N2, S2|_] ->
        atom_string(Name2, N2),
        atom_string(Surname2, S2),
        full_name(Name2, Surname2, FullName2)
    ; write('Invalid input!'), nl, fail ),

    (
        person(G2, FullName2, _, BY2, DY2, _, _, _) ->
            % Kişi zaten kayıtlı, doğrudan doğrulama ve evlilik ekleme kısmına geç
            validate_and_add_marriage(FullName1, FullName2)
        ;
        % Kişi yoksa bilgilerini al ve ekle
        write('Type new person birth year: '), flush_output,
        read_line_to_string(user_input, BYStr),
        number_string(BY2, BYStr),
        write('Type new person death year (0 if alive): '), flush_output,
        read_line_to_string(user_input, DYStr),
        number_string(DY2, DYStr),
        write('Type new person gender (m/f): '), flush_output,
        read_line_to_string(user_input, GStr),
        atom_string(GAtom, GStr),
        (GAtom = m ; GAtom = f) -> true ; write('Invalid gender!'), nl, fail,

        % Ana kişiyi biliyoruz, ek olarak diğer kişinin anne-baba bilgileri bilinmiyor
        assertz(person(GAtom, FullName2, Surname2, BY2, DY2, unknown, unknown, [])),

        % Devam et
        validate_and_add_marriage(FullName1, FullName2)
    ).
    ).
forbidden_marriage(P1, P2) :-
    P1 == P2,
    write('Invalid Marriage: Cannot marry oneself.'), nl, fail.

forbidden_marriage(P1, P2) :-
    already_married(P1),
    write(P1), write(' is already married.'), nl, fail.

forbidden_marriage(P1, P2) :-
    already_married(P2),
    write(P2), write(' is already married.'), nl, fail.

forbidden_marriage(P1, _) :-
    person(_, P1, _, _, D1, _, _, _),
    D1 \== 0, D1 \= '0',
    write(P1), write(' is deceased and cannot marry.'), nl, fail.

forbidden_marriage(_, P2) :-
    person(_, P2, _, _, D2, _, _, _),
    D2 \== 0, D2 \= '0',
    write(P2), write(' is deceased and cannot marry.'), nl, fail.

% Yakın akrabalık kontrolü
forbidden_marriage(P1, P2) :-
    ( close_relative(P1, P2) -> true ; close_relative(P2, P1) ),
    write('Invalid Marriage: Close relatives cannot marry.'), nl, fail.

% İlişki türüne göre özel mesajlı yasaklama
forbidden_marriage(P1, P2) :-
    ( find_relation(P1, P2, Rel)
    ; find_relation(P2, P1, Rel) ),
    forbidden_relation(Rel, Message),
    write('Invalid Marriage: '), write(Message), nl, fail.

% Turkish explanations of forbidden relationships
forbidden_relation('Anne',         'Anne-Ogul').
forbidden_relation('Baba',         'Baba-Kız').
forbidden_relation('Kiz',          'kız-Baba').
forbidden_relation('Ogul',         'ogul-anne').
forbidden_relation('Amca',         'amca-yegen').
forbidden_relation('Dayi',         'dayı-yegen').
forbidden_relation('Teyze',        'teyze-yegen').
forbidden_relation('Hala',         'hala-yegen').
forbidden_relation('Yegen',        'yegen ile evlenemez').
forbidden_relation('Abi',          'abi-kız kardes').
forbidden_relation('Abla',         'abla- erkek kardes').
forbidden_relation('Erkek Kardes', 'erkek kardes-abla').
forbidden_relation('Kiz Kardes',   'kız kardes-abi').
forbidden_relation('Yenge',        'yengen'), nl, fail
    ; true).  

% Kişi bilgisi gösterme
show_person_info :-
    write('Please type the person name and surname: '), flush_output,
    read_line_to_string(user_input, Input),
    split_string(Input, " ", "", Words),
    ( Words = [NameStr, SurnameStr|_] ->
        atom_string(Name, NameStr),
        atom_string(Surname, SurnameStr),
        full_name(Name, Surname, FullName),
        ( person(_, FullName, _, _, _, _, _, _) ->
            age(FullName, Age),
            level(FullName, Level),
            child_count(FullName, ChildCount),
            ( alive(FullName) -> Status = 'alive' ; Status = 'dead' ),
            format('Age: ~w~n', [Age]),
            format('Level: ~w~n', [Level]),
            format('Total child: ~w~n', [ChildCount]),
            format('dead or alive?: ~w~n', [Status])
        ; write('Person not found!'), nl
        )
    ; write('Invalid input! Please type name and surname properly.'), nl
    ).

% YENİ VE GELİŞTİRİLMİŞ SOY AĞACI GÖRÜNTÜLEME
display_tree :-
    % Tüm seviyeleri bul
    findall(L, (person(_, _, _, _, _, _, _, _), level(_, L)), AllLevels),
    list_to_set(AllLevels, Levels),
    sort(Levels, SortedLevels),
    % Her seviyeyi sırayla yazdır
    print_all_levels(SortedLevels).

% Tüm seviyeleri yazdır
print_all_levels([]).
print_all_levels([CurrentLevel|RestLevels]) :-
    format('---LEVEL ~w---~n', [CurrentLevel]),
    % Bu seviyedeki tüm kişileri bul
    findall(Person, level(Person, CurrentLevel), PeopleAtLevel),
    % Kişileri ve eşlerini yazdır
    print_people_with_spouses(PeopleAtLevel),
    nl,
    print_all_levels(RestLevels).

find_spouse(Person, Spouse) :-
    (marriage(Person, Spouse) ; marriage(Spouse, Person)),
    Spouse \= Person, !.
find_spouse(_, '').

% İlişki sorgulama (gelişmiş Turkish version)
find_relationship :-
    write('Type first person name and surname: '),
    read_line_to_string(user_input, Input1),
    split_string(Input1, " ", "", Parts1),
    ( Parts1 = [N1,S1|_] ->
        atom_string(Name1, N1),
        atom_string(Surname1, S1),
        full_name(Name1, Surname1, FullName1)
    ; write('Invalid input!'), nl, fail ),
    write('Type second person name and surname: '),
    read_line_to_string(user_input, Input2),
    split_string(Input2, " ", "", Parts2),
    ( Parts2 = [N2,S2|_] ->
        atom_string(Name2, N2),
        atom_string(Surname2, S2),
        full_name(Name2, Surname2, FullName2)
    ; write('Invalid input!'), nl, fail ),
    ( person(_, FullName1, _, _, _, _, _, _),
      person(_, FullName2, _, _, _, _, _, _) ->
        findall(Relation,
            find_relation(FullName1, FullName2, Relation),
            Relations),
        ( Relations \= [] ->
            format('Relation(s) between ~w and ~w:~n', [FullName1, FullName2]),
            print_relations(Relations)
        ; write('No direct relation found.'), nl )
    ; write('One or both persons not found.'), nl ).

print_relations([]).
print_relations([H|T]) :-
    format('- ~w~n', [H]),
    print_relations(T).

% Kişi ekleme-güncelleme menüsü
add_or_update_person :-
    nl,
    write('1. Add person'), nl,
    write('2. Update person'), nl,
    write('0. Cancel'), nl,
    write('Please choose an operation: '), flush_output,
    read_line_to_string(user_input, ChoiceStr),
    ( number_string(Choice, ChoiceStr) -> true ; write('Invalid input.'), nl, add_or_update_person ),
    process_add_update_choice(Choice).

process_add_update_choice(1) :- add_person.
process_add_update_choice(2) :- update_person.
process_add_update_choice(0) :- write('Operation canceled.'), nl.
process_add_update_choice(_) :- write('Invalid choice.'), nl, add_or_update_person.

add_person :-
    write('Type father name and surname: '), flush_output,
    read_line_to_string(user_input, FatherInput),
    split_string(FatherInput, " ", "", FatherParts),
    ( FatherParts = [FN, FS] ->
        atom_string(FatherName, FN),
        atom_string(FatherSurname, FS),
        full_name(FatherName, FatherSurname, FatherFullName)
    ; write('Invalid father name input!'), nl, fail ),

    write('Type mother name and surname: '), flush_output,
    read_line_to_string(user_input, MotherInput),
    split_string(MotherInput, " ", "", MotherParts),
    ( MotherParts = [MN, MS] ->
        atom_string(MotherName, MN),
        atom_string(MotherSurname, MS),
        full_name(MotherName, MotherSurname, MotherFullName)
    ; write('Invalid mother name input!'), nl, fail ),

    write('Type child name and surname: '), flush_output,
    read_line_to_string(user_input, ChildInput),
    split_string(ChildInput, " ", "", ChildParts),
    ( ChildParts = [CN, CS] ->
        atom_string(ChildName, CN),
        atom_string(ChildSurname, CS),
        full_name(ChildName, ChildSurname, ChildFullName)
    ; write('Invalid child name input!'), nl, fail ),

    write('Type birth year (integer): '), flush_output,
    read_line_to_string(user_input, BirthYearStr),
    ( number_string(BirthYear, BirthYearStr) -> true ; write('Invalid birth year!'), nl, fail ),

    write('Type death year (0 if alive): '), flush_output,
    read_line_to_string(user_input, DeathYearStr),
    ( number_string(DeathYear, DeathYearStr) -> true ; write('Invalid death year!'), nl, fail ),

    write('Type child gender (m/f): '), flush_output,
    read_line_to_string(user_input, GenderStr),
    ( (GenderStr = "m"; GenderStr = "f") -> true ; write('Invalid gender!'), nl, fail ),

    ( GenderStr = "m" -> Gender = male ; Gender = female ),

    % Eğer ebeveynler kayıtlı değilse, uyar
    ( person(_, FatherFullName, _, _, _, _, _, _) -> true ; write('Warning: Father not found in records.'), nl ),
    ( person(_, MotherFullName, _, _, _, _, _, _) -> true ; write('Warning: Mother not found in records.'), nl ),

    assertz(person(Gender, ChildFullName, ChildSurname, BirthYear, DeathYear, FatherFullName, MotherFullName, '')),
    write('Person added successfully.'), nl.

update_person :-
    nl,
    write('1. Update the birth year of someone'), nl,
    write('2. Update the death year of someone'), nl,
    write('0. Cancel'), nl,
    write('Enter your choice: '), flush_output,
    read_line_to_string(user_input, Choice),
    handle_update_choice(Choice).

handle_update_choice("0") :-
    write('Cancelled.'), nl.

handle_update_choice("1") :-
    write('Enter the name of the person you want to update (Name Surname): '), flush_output,
    read_line_to_string(user_input, Input),
    split_string(Input, " ", "", Parts),
    ( Parts = [N, S] ->
        atom_string(Name, N),
        atom_string(Surname, S),
        full_name(Name, Surname, FullName)
    ; write('Invalid input!'), nl, fail ),
    ( person(G, FullName, Sur, BY, DY, F, M, C) ->
        retract(person(G, FullName, Sur, BY, DY, F, M, C)),
        write('Enter the new birth year: '), flush_output,
        read_line_to_string(user_input, BYStr),
        ( number_string(NewBY, BYStr) ->
            assertz(person(G, FullName, Sur, NewBY, DY, F, M, C)),
            write('Birth year updated successfully.'), nl
        ; write('Invalid year. Update cancelled.'), nl,
          assertz(person(G, FullName, Sur, BY, DY, F, M, C)) )
    ; write('Person not found.'), nl ).

handle_update_choice("2") :-
    write('Enter the name of the person you want to update (Name Surname): '), flush_output,
    read_line_to_string(user_input, Input),
    split_string(Input, " ", "", Parts),
    ( Parts = [N, S] ->
        atom_string(Name, N),
        atom_string(Surname, S),
        full_name(Name, Surname, FullName)
    ; write('Invalid input!'), nl, fail ),
    ( person(G, FullName, Sur, BY, DY, F, M, C) ->
        retract(person(G, FullName, Sur, BY, DY, F, M, C)),
        write('Enter the new death year (0 if alive): '), flush_output,
        read_line_to_string(user_input, DYStr),
        ( number_string(NewDY, DYStr) ->
            assertz(person(G, FullName, Sur, BY, NewDY, F, M, C)),
            write('Death year updated successfully.'), nl
        ; write('Invalid year. Update cancelled.'), nl,
          assertz(person(G, FullName, Sur, BY, DY, F, M, C)) )
    ; write('Person not found.'), nl ).

handle_update_choice(_) :-
    write('Invalid choice.'), nl,
    update_person.

% Ana menü
main_menu :-
    nl,
    write('1. Ask relation'), nl,
    write('2. Add/Update person'), nl,
    write('3. Get information of any person'), nl,
    write('4. Print the family tree'), nl,
    write('5. Add marriage'), nl,
    write('6. Terminate the program'), nl,
    write('Please choose an operation! |: '), flush_output,
    read_line_to_string(user_input, ChoiceStr),
    ( number_string(Choice, ChoiceStr) -> true ; write('Invalid input, please enter a number.'), nl, main_menu ),
    process_choice(Choice).

process_choice(1) :- find_relationship, main_menu.
process_choice(2) :- add_or_update_person, main_menu.
process_choice(3) :- show_person_info, main_menu.
process_choice(4) :- display_tree, main_menu.
process_choice(5) :- add_marriage, main_menu.
process_choice(6) :- write('Program terminated. Goodbye!'), nl.
process_choice(_) :- write('Invalid choice, please try again.'), nl, main_menu.

% Programı başlat
start :- main_menu.

% Test verileri eklemek için
add_test_data :-
    % Murat ve Sedanur'un çocukları (Level 1)
    assertz(person(male, 'Ahmet Aslan', 'Aslan', 1964, 0, 'Murat Aslan', 'Sedanur Aslan', '')),
    assertz(person(female, 'Buse Aslan', 'Aslan', 1962, 0, 'Murat Aslan', 'Sedanur Aslan', '')),
    
    % Ahmet'in karısı
    assertz(person(female, 'Mehtap Duman', 'Duman', 1964, 0, '', '', '')),
    assertz(marriage('Ahmet Aslan', 'Mehtap Duman')),
    
    % Buse'nin kocası
    assertz(person(male, 'Faruk Kaya', 'Kaya', 1960, 0, '', '', '')),
    assertz(marriage('Buse Aslan', 'Faruk Kaya')),
    
   level(FullName, 0) :-
    person(_, FullName, _, _, _, '', '', _).

level(FullName, Level) :-
    person(_, FullName, _, _, _, Father, Mother, _),
    (Father \= '' -> level(Father, ParentLevel)
     ; Mother \= '' -> level(Mother, ParentLevel)),
    Level is ParentLevel + 1.
    
    % Her seviyeyi yazdır
    print_levels(SortedLevels).

print_levels([]).
print_levels([Level|Rest]) :-
    format('---LEVEL ~w---~n', [Level]),
    findall(Person, (person(_, Person, _, _, _, _, _, _), level(Person, Level)), PeopleAtLevel),
    print_people_with_spouses(PeopleAtLevel),
    nl,
    print_levels(Rest).

print_people_with_spouses([]).
print_people_with_spouses([Person|Rest]) :-
    find_spouse(Person, Spouse),
    (Spouse == '' -> 
        format('~w~n', [Person])
    ; 
        % Eşi zaten başka bir seviyede basılmadıysa bas
        (level(Person, PL), level(Spouse, SL), PL =< SL ->
            format('~w + ~w~n', [Person, Spouse])
        ;
            format('~w~n', [Person])
        )
    ),
    print_people_with_spouses(Rest).