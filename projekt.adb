-- dzielona.adb
-- przykład użycia zmiennej dzielonej

with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Float_Text_IO;
use Ada.Float_Text_IO;

with Ada.Calendar;
use Ada.Calendar;
with Ada.Numerics.Float_Random;

with Ada.Strings;
use Ada.Strings;
with Ada.Strings.Fixed;
use Ada.Strings.Fixed;

with Ada.Exceptions;
use Ada.Exceptions;



procedure Projekt is
  
  Koniec : Boolean := False with Atomic;
  
  type Stany is (Duzo, Malo);
  Stan : Stany := Malo with Atomic;
  
  type Atrybuty is (Czysty, Jasny, Podkreslony, Negatyw, Migajacy, Szary);

  protected Ekran  is
    procedure Pisz_XY(X,Y: Positive; S: String; Atryb : Atrybuty := Czysty);
    procedure Pisz_Float_XY(X, Y: Positive; 
                            Num: Float; 
                            Pre: Natural := 3; 
                            Aft: Natural := 2; 
                            Exp: Natural := 0; 
                            Atryb : Atrybuty := Czysty);
    procedure Czysc;
    procedure Tlo;
  end Ekran;
  
  protected body Ekran is
    -- implementacja dla Linuxa i macOSX
    function Atryb_Fun(Atryb : Atrybuty) return String is 
      (case Atryb is 
       when Jasny => "1m", when Podkreslony => "4m", when Negatyw => "7m",
       when Migajacy => "5m", when Szary => "2m", when Czysty => "0m"); 
       
    function Esc_XY(X,Y : Positive) return String is 
      ( (ASCII.ESC & "[" & Trim(Y'Img,Both) & ";" & Trim(X'Img,Both) & "H") );   
       
    procedure Pisz_XY(X,Y: Positive; S: String; Atryb : Atrybuty := Czysty) is
      Przed : String := ASCII.ESC & "[" & Atryb_Fun(Atryb);              
    begin
      Put( Przed);
      Put( Esc_XY(X,Y) & S);
      Put( ASCII.ESC & "[0m");
    end Pisz_XY;  
    
    procedure Pisz_Float_XY(X, Y: Positive; 
                            Num: Float; 
                            Pre: Natural := 3; 
                            Aft: Natural := 2; 
                            Exp: Natural := 0; 
                            Atryb : Atrybuty := Czysty) is
                              
        Przed_Str : String := ASCII.ESC & "[" & Atryb_Fun(Atryb);              
    begin
      Put( Przed_Str);
      Put( Esc_XY(X, Y) );
      Put( Num, Pre, Aft, Exp);
      Put( ASCII.ESC & "[0m");
    end Pisz_Float_XY; 
    
    procedure Czysc is
    begin
      Put(ASCII.ESC & "[2J");
    end Czysc;   
    
    procedure Tlo is
    begin
        Ekran.Czysc;

        Ekran.Pisz_XY(1,1,"Program przedstawia mini grę, która ma na celu przedstawienie zagrożeń związanych z niesprawnym kotłem w mieszkaniu. Czad jest dużym zagrożeniem");
        Ekran.Pisz_XY(1,2,"dla zdrowia i życia użytkownikownika. Podczas gry użytkownik może kontrolować piec oraz wywietrzniki, jednak należy uważać, ponieważ ze wzrostem");
        Ekran.Pisz_XY(1,3,"mocy pieca wzrasta ilość wydzielanego tlenku węgla. Jeżeli czadu będzie za dużo można go wywietrzyć używając wentylacji. Miłej zabawy :D");


        Ekran.Pisz_XY(1,5,"+======= Warunki =======+");

        Ekran.Pisz_XY(1,7,"+== Wewnątrz ==+");
        Ekran.Pisz_XY(3,8,"Temperatura [C]:");
        Ekran.Pisz_XY(3,9,"Ilość czadu [ppm]:");

        Ekran.Pisz_XY(1,11,"+== Na zewnątrz ==+");
        Ekran.Pisz_XY(3,12,"Temperatura [C]:");

        -- PIEC
        Ekran.Pisz_XY(40,5,"+== PIEC ==+");

        Ekran.Pisz_XY(40,7,"Działa:");
        Ekran.Pisz_XY(40,8,"Moc:");

        -- Czujnik czadu
        Ekran.Pisz_XY(60,5,"+== CZUJNIK CZADU ==+");

        Ekran.Pisz_XY(60,7,"Stężenie czadu [ppm/m3]:");

        -- powiadomienia z Czujnika
        Ekran.Pisz_XY(60,10,"+==== POWIADOMIENIA Z CZUJNIKA CZADU ====+");

        -- Punktacja
        Ekran.Pisz_XY(100,5,"+== PUNKTACJA ==+");

        Ekran.Pisz_XY(100,7,"Punkty: ");

        Ekran.Pisz_XY(1,16,"+= Q-koniec, X-Zwiększ moc pieca, Z-Zamniejsz moc pieca, S-Wywietrz pomieszczenie =+");

        
    end Tlo; 
        
    end Ekran;
    
-- Pomieszczenie (dane dzielone między wątki)


    subtype Moc is Integer range 0 .. 5;

    Kubatura : constant Float := 50.0;
    Punkty : Float := 0.0;

    Protected Pomieszczenie is
        procedure DodajCzadu(CZ : in Integer);
        procedure UsunCzad;
        function CzytajCzad return Integer;
        function CzytaCzadNaMetrSzescienny return Integer;
        procedure ZwiekszMoc;
        procedure ZmniejszMoc;
        procedure WylaczPiec;
        function CzytajMoc return Integer;
        procedure Grzej(Energia : in Float);
        function CzytajTemperature return Float;
        procedure ZmienTemperatureZewnetrzna(Zmiana_Temp : in Float);
        function CzytajTemperatureZewnetrzna return Float;

    private
        Czad : Integer := 0;
        M : Moc := 0;
        Temp : Float := 0.0;
        Temp_zew : Float := 0.0;
    end Pomieszczenie;

    Protected body Pomieszczenie is
        procedure DodajCzadu(CZ : in Integer) is 
        begin
            Czad := Czad + CZ;
        end DodajCzadu;

        procedure UsunCzad is
        begin
            Czad := 0;
            Temp := Temp_zew;
        end UsunCzad;

        function CzytajCzad return Integer is
           (Czad);

        function CzytaCzadNaMetrSzescienny return Integer is
        begin
            return (Czad/Integer(Kubatura));
        end CzytaCzadNaMetrSzescienny;

        procedure ZwiekszMoc is
        begin
            if M < 5 then M:=M+1; end if;
        end ZwiekszMoc;

        procedure ZmniejszMoc is
        begin
            If M > 0 then M:=M-1; end if;
        end ZmniejszMoc;

        procedure WylaczPiec is
        begin
            M := 0;
        end WylaczPiec;

        function CzytajMoc return Integer is
        begin
            return M;
        end CzytajMoc;

        procedure Grzej (Energia : in Float) is
        begin
            Temp := Temp + (Energia/0.32)/Kubatura;
        end Grzej;

        function CzytajTemperature return Float is
        begin
            return Temp;
        end CzytajTemperature;

        procedure ZmienTemperatureZewnetrzna(Zmiana_Temp : in Float) is
        Temp_zew_t : Float;
        begin
            Temp_zew_t := Temp_zew + Zmiana_Temp;
            if Temp_zew_t < 0.0 then Temp_zew := Temp_zew + Zmiana_Temp;
            end if;
        end ZmienTemperatureZewnetrzna;

        function CzytajTemperatureZewnetrzna return Float is 
        begin
            return Temp_zew;
        end CzytajTemperatureZewnetrzna;

    end Pomieszczenie;

-- Wątki pieca, czujnika, pomieszczenie.

    task Punktacja;

    task body Punktacja is
    Nastepny     : Ada.Calendar.Time;
    Okres        : constant Duration := 0.5;
    Przesuniecie : constant Duration := 0.0;
    begin
        Nastepny := Clock + Przesuniecie;
        loop
            delay until Nastepny;
            Punkty := Punkty + Pomieszczenie.CzytajTemperature*5.0 - Float(Pomieszczenie.CzytaCzadNaMetrSzescienny);
            exit when Koniec = True;
            Nastepny := Nastepny + Okres;
        end loop;
        exception
        when E:others =>
            Put_Line("Error: Zadanie Punktacja");
            Put_Line(Exception_Name (E) & ": " & Exception_Message (E));
    end Punktacja;

    task Wentylacja is
        entry Wietrz (Dlugosc : in Integer);
    end Wentylacja;

    task body Wentylacja is
    Nastepny     : Ada.Calendar.Time;
    Okres        : constant Duration := 0.5;
    Przesuniecie : constant Duration := 0.05;
    begin
        Nastepny := Clock + Przesuniecie;
        loop
            select
                accept Wietrz (Dlugosc : in Integer) do
                    --for K in 1..Dlugosc loop
                        Pomieszczenie.Grzej(-0.32*Kubatura*(5.0));
                        Pomieszczenie.UsunCzad;
                    --end loop;
                end Wietrz;
                or
                terminate;
            end select;
            exit when Koniec = True;
            Nastepny := Nastepny + Okres;
        end loop;
        exception
        when E:others =>
            Put_Line("Error: Zadanie Wentylacja");
            Put_Line(Exception_Name (E) & ": " & Exception_Message (E));
    end Wentylacja;

    task Czujnik_Czadu;

    task body Czujnik_Czadu is 
    Nastepny     : Ada.Calendar.Time;
    Okres        : constant Duration := 0.5;
    Przesuniecie : constant Duration := 0.1;
    begin
        Nastepny := Clock + Przesuniecie;
        loop
            delay until Nastepny;
            if (Pomieszczenie.CzytaCzadNaMetrSzescienny > 20000) then
                Wentylacja.Wietrz(10);
            end if;
            exit when Koniec = True;
            Nastepny := Nastepny + Okres;
        end loop;
        exception
        when E:others =>
            Put_Line("Error: Zadanie Czujnik Czadu");
            Put_Line(Exception_Name (E) & ": " & Exception_Message (E));
    end Czujnik_Czadu;

    task Przenikanie;

    task body Przenikanie is 
    Nastepny     : Ada.Calendar.Time;
    Okres        : constant Duration := 0.5;
    Przesuniecie : constant Duration := 0.15;

    wsp_przenikania : constant Float := 0.23;
    powierzchnia : constant Float := 0.2;
    En : Float;
    begin 
        Nastepny := Clock + Przesuniecie;
        loop
            delay until Nastepny;
            En := wsp_przenikania * powierzchnia * (Pomieszczenie.CzytajTemperatureZewnetrzna -
            Pomieszczenie.CzytajTemperature);
            -- Ekran.Pisz_Float_XY(40,8,En);
            Pomieszczenie.Grzej(En);
            exit when Koniec = True;
            Nastepny := Nastepny + Okres;
        end loop;
        exception
        when E:others =>
            Put_Line("Error: Zadanie Przenikanie");
            Put_Line(Exception_Name (E) & ": " & Exception_Message (E)); 
    end Przenikanie;

    task WarunkiZewnetrzne;

    task body WarunkiZewnetrzne is
    use Ada.Numerics.Float_Random;
    Gen : Generator;
    R : Float;
    Nastepny     : Ada.Calendar.Time;
    Okres        : constant Duration := 0.5;
    Przesuniecie : constant Duration := 0.2;
    begin
        -- Reset(Gen);
        Nastepny := Clock + Przesuniecie;
        Reset(Gen);
        loop
            delay until Nastepny;
            
            R := 5.0*(Random(Gen)-0.5);
            if (Pomieszczenie.CzytajTemperatureZewnetrzna + R < -40.0) or
                (Pomieszczenie.CzytajTemperatureZewnetrzna + R > 30.0) 
                then
                R := -R;
            end if;

            Pomieszczenie.ZmienTemperatureZewnetrzna(R);

            exit when Koniec = True;
            Nastepny := Nastepny + Okres;
        end loop;
        exception
        when E:others =>
            Put_Line("Error: Zadanie Warunki Zewnetrzne");
            Put_Line(Exception_Name (E) & ": " & Exception_Message (E)); 
    end WarunkiZewnetrzne;


    task Piec;

    task body Piec is 
    Nastepny     : Ada.Calendar.Time;
    Okres        : constant Duration := 0.5;
    Przesuniecie : constant Duration := 0.25;
    begin 
        Nastepny := Clock + Przesuniecie;
        loop
            delay until Nastepny;
            if Pomieszczenie.CzytajMoc = 0 then
                Pomieszczenie.DodajCzadu(0); 
            elsif Pomieszczenie.CzytajMoc = 1 then
                Pomieszczenie.Grzej(1.0);
                Pomieszczenie.DodajCzadu(5);
            elsif Pomieszczenie.CzytajMoc = 2 then 
                Pomieszczenie.Grzej(2.0);
                Pomieszczenie.DodajCzadu(15);
            elsif Pomieszczenie.CzytajMoc = 3 then 
                Pomieszczenie.Grzej(3.0);
                Pomieszczenie.DodajCzadu(75);
            elsif Pomieszczenie.CzytajMoc = 4 then
                Pomieszczenie.Grzej(4.0);
                Pomieszczenie.DodajCzadu(300);
            elsif Pomieszczenie.CzytajMoc = 5 then 
                Pomieszczenie.Grzej(5.0);
                Pomieszczenie.DodajCzadu(5000);
            end if;
            exit when Koniec = True;
            Nastepny := Nastepny + Okres;
        end loop;
        exception
        when E:others =>
            Put_Line("Error: Zadanie Piec");
            Put_Line(Exception_Name (E) & ": " & Exception_Message (E)); 
    end Piec;

    task Pieckontroler;

    task body Pieckontroler is
    Nastepny     : Ada.Calendar.Time;
    Okres        : constant Duration := 0.5;
    Przesuniecie : constant Duration := 0.3;
    Zn : Character;
    begin
        Nastepny := Clock + Przesuniecie;
        loop
            Get_Immediate(Zn);
            exit when Zn in 'q'|'Q';
            if Zn in 'X'|'x' then Pomieszczenie.ZwiekszMoc;
            elsif Zn in 'Z'|'z' then Pomieszczenie.ZmniejszMoc;
            elsif Zn in 'S'|'s' then Wentylacja.Wietrz(10);
            end if;
        end loop;
        Koniec := True;
        exception
        when E:others =>
            Put_Line("Error: Zadanie Piec Kontroler");
            Put_Line(Exception_Name (E) & ": " & Exception_Message (E)); 
    end Pieckontroler;



    subtype Zakres is Integer range 1..10; 
    --Zadania: array(Zakres) of Zadanie;
    Nastepny     : Ada.Calendar.Time;
    Okres        : constant Duration := 0.5;
    Przesuniecie : constant Duration := 0.35;
begin
    Nastepny := Clock + Przesuniecie;
    Ekran.Tlo;
    loop
    
        delay until Nastepny;
        Ekran.Tlo;
        exit when Koniec = True;
        
        Ekran.Pisz_Float_XY(22,8,Pomieszczenie.CzytajTemperature, Atryb=>Jasny);
        Ekran.Pisz_XY(22, 9, Pomieszczenie.CzytajCzad'Img, Atryb=>Jasny);
        Ekran.Pisz_Float_XY(22,12,Pomieszczenie.CzytajTemperatureZewnetrzna, Atryb=>Jasny);

        if Pomieszczenie.CzytajMoc = 0 then 
            Ekran.Pisz_XY(48,7,"OFF", Atryb=>Jasny);
        else
            Ekran.Pisz_XY(48,7,"ON ", Atryb=>Jasny);
        end if;
        Ekran.Pisz_XY(47,8,Pomieszczenie.CzytajMoc'Img, Atryb=>Jasny);

        Ekran.Pisz_XY(85,7,Pomieszczenie.CzytaCzadNaMetrSzescienny'Img, Atryb=>Jasny);

        if (Pomieszczenie.CzytaCzadNaMetrSzescienny >= 100 and Pomieszczenie.CzytaCzadNaMetrSzescienny < 400) then 
            Ekran.Pisz_XY(60,12,"lekki ból głowy przy ekspozycji przez 2–3 godziny.", Atryb=>Czysty);
        elsif (Pomieszczenie.CzytaCzadNaMetrSzescienny >= 400 and Pomieszczenie.CzytaCzadNaMetrSzescienny < 800) then 
            Ekran.Pisz_XY(60,12,"silny ból głowy zaczynający się ok. 1 godzinę po", Atryb=>Czysty);
            Ekran.Pisz_XY(60,13,"wdychaniu tego stężenia.", Atryb=>Czysty);
        elsif (Pomieszczenie.CzytaCzadNaMetrSzescienny >= 800 and Pomieszczenie.CzytaCzadNaMetrSzescienny < 1600) then 
            Ekran.Pisz_XY(60,12,"zawroty głowy, wymioty i konwulsje po 45 minutach", Atryb=>Podkreslony);
            Ekran.Pisz_XY(60,13,"wdychania; po dwóch godzinach trwała śpiączka.", Atryb=>Podkreslony);
        elsif (Pomieszczenie.CzytaCzadNaMetrSzescienny >= 1600 and Pomieszczenie.CzytaCzadNaMetrSzescienny < 3200) then 
            Ekran.Pisz_XY(60,12,"silny ból głowy, wymioty, konwulsje po 20 minutach;", Atryb=>Jasny);
            Ekran.Pisz_XY(60,13,"zgon po dwóch godzinach", Atryb=>Jasny);
        elsif (Pomieszczenie.CzytaCzadNaMetrSzescienny >= 3200 and Pomieszczenie.CzytaCzadNaMetrSzescienny < 6400) then 
            Ekran.Pisz_XY(60,12,"intensywny ból głowy i wymioty po 5–10 minutach;", Atryb=>Jasny);
            Ekran.Pisz_XY(60,13,"zgon po 30 minutach", Atryb=>Jasny);
        elsif (Pomieszczenie.CzytaCzadNaMetrSzescienny >= 6400 and Pomieszczenie.CzytaCzadNaMetrSzescienny < 12800) then 
            Ekran.Pisz_XY(60,12,"ból głowy i wymioty po 1–2 minutach; zgon w niecałe 20 minut", Atryb=>Negatyw);
        elsif (Pomieszczenie.CzytaCzadNaMetrSzescienny >= 12800) then 
            Ekran.Pisz_XY(60,12,"utrata przytomności po 2–3 wdechach; śmierć po 3 minutach.", Atryb=>Negatyw);
        end if;

        Ekran.Pisz_Float_XY(108,7,Punkty, Atryb=>Jasny);

        Ekran.Pisz_XY(1,20,"");

        Nastepny := Nastepny + Okres;
    end loop;
    Ekran.Czysc;
end Projekt;
	  	