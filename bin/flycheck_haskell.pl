#!/usr/bin/env perl
# flycheck_haskell.pl

### Please rewrite the following 3 variables
### ($ghc, @ghc_options and @ghc_packages)

$ghc = '/usr/bin/ghc'; # where is ghc
@ghc_options  = ('-Wall');	 # e.g. ('-fglasgow-exts')
@ghc_packages = ();			 # e.g. ('QuickCheck')

### the following should not been edited ###

use File::Temp qw /tempfile tempdir/;
File::Temp->safe_level( File::Temp::HIGH );

($source, $base_dir) = @ARGV;

$source =~ s/ /\\ /g;
$base_dir =~ s/ /\\ /g;

@command = ($ghc,
			'--make',
			'-fbyte-code',
			"-i$base_dir",
			"-i$base_dir/..",
			"-i$base_dir/../..",
			"-i$base_dir/../../..",
			"-i$base_dir/../../../..",
			"-i$base_dir/../../../../..",
			$source);

while(@ghc_options) {
	push(@command, shift @ghc_options);
}

while(@ghc_packages) {
	push(@command, '-package');
	push(@command, shift @ghc_packages);
}

$dir = tempdir( CLEANUP => 1 );
($fh, $filename) = tempfile( DIR => $dir );

system("@command >$filename 2>&1");
open(MESSAGE, $filename);
while(<MESSAGE>) {
	if(/(^\S+\.hs|\.lhs)(:\d*:\d*:)\s?(.*)/) {
		print "\n";
		print $1;
		print $2;
		$rest = $3;
		chomp $rest;
		print $rest;
		next;
	}
	if(/\s+(.*)/) {
		$rest = $1;
		chomp $rest;
		print $rest;
		print " ";
		next;
	}
}
close($fh);
print "\n";
