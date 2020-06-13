#/usr/bin/perl

$name = shift;
$bin  = shift;


print "$name :: String\n";
print "$name =\n";

if ($bin eq '-bin') {
    @file = <STDIN>;
    @file = map { split(//,$_); } @file;
    @file = map { ord $_; } @file;
    $file = join(",\n\t\t",@file);
    print "\t map toEnum [ $file ]\n";
}  else {
    foreach (<STDIN>) {
	chop($_);
	s/\\/\\\\/g;
	s/\"/\\"/g;
	s/\t/\\t/g;
       print "\t\"$_\\n\" ++ \n";
    }
    print "\t\"\" -- end of $name\n"
}

