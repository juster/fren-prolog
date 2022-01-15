#!/usr/bin/perl

while(<>){
  if($. == 305 ... $. == 316){
    if(/mot\(nom\(([^)]+)\), *([^)]+)/){
      $nom = $1;
      $ang = $2;
      $nom =~ s/(une?|les?|las?) +//i;
      $nom =~ s/'//g if($nom !~ / /);
      $ang =~ s/(an?|the) +//i;
      print "mot($nom, $ang)\n";
      next;
    }
  }
  print;
}
