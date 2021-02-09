use strict;
use warnings;

# This file tests interactions with locale and threads

BEGIN {
    chdir 't' if -d 't';
    require './test.pl';
    set_up_inc('../lib');
    require './loc_tools.pl';
    skip_all("No locales") unless locales_enabled();
    skip_all_without_config('useithreads');
    $| = 1;
    eval { require POSIX; POSIX->import(qw(errno_h locale_h  unistd_h )) };
    if ($@) {
        skip_all("could not load the POSIX module"); # running minitest?
    }
}

use Time::HiRes qw(time usleep);

my $thread_count = 3;
my $iterations = 1000;
my $max_result_length = 10000;

# Estimate as to how long to allow a thread to be ready to roll after
# creation, so as to try to get all the threads to start as simultaneously as
# possible
my $per_thread_startup = .02;

# For use in tuning the above value
my $die_on_negative_sleep = 0; #1;

# We don't need to test every possible errno, but setting it to negative does
# so
my $max_message_catalog_entries = 10;

# reset the locale environment
delete local @ENV{'LANGUAGE', 'LANG', (grep /^LC_[A-Z]+$/, keys %ENV)};

sub ordering ()
{
    ($a =~ /[@.]/
    ? ($b !~ /[@.]/
      ? -1             # Choose a if it has [.@] and $b doesn't
      : ($a =~ /885915?\b/  # Both have [@.]
        ? ($b !~ /885915?\b/
          ? 1          # Prioritize non-Latin1
          : $b cmp $a) # End of alphabet more likely to be
                       # exotic, chinese etc.
        : ($b =~ /885915?\b/
          ? -1
          : ($a =~ /[.]/  # Both are Latin1 with [@.]
            ? ($b !~ /[.]/
              ? -1       # Prioritize . over @
              : $b cmp $a)   # Both have dot
            : ($b =~ /[.]/   # $a has @, not dot
              ? 1
              : $b cmp $a)))))
    : ($b =~ /[@.]/     # a doesn't have [@.]
      ? 1
      : $b cmp $a));
}

# For each category, create a list of its valid locales.  These will likely be
# the same for all categories, but full generality is easy, so do it.
my %valid_locales;
my @prioritized;

foreach my $category (valid_locale_categories()) {
    my @non_utf8_locales = sort ordering find_locales($category);
    next unless @non_utf8_locales;

    @non_utf8_locales = grep { $_ ne 'C' } @non_utf8_locales;

    my @utf8_locales =
        reverse sort { $a =~ /^en_/ ? -1 : $b =~ /^en_/ ? 1 : $a cmp $b }
                                                    find_utf8_ctype_locales();
    foreach my $utf8 (@utf8_locales) {
        @non_utf8_locales = grep { $_ ne $utf8 } @non_utf8_locales;
    }

    my $C_UTF8_re = qr/^ C \. UTF -?8 $ /x;
    my $found_C_UTF8 = 0;
    if (grep { $_ =~ $C_UTF8_re } @utf8_locales) {
        my $found_C_UTF8 = 1;
        @utf8_locales = grep { $_ !~ $C_UTF8_re } @utf8_locales;
    }

    @prioritized = ('C', shift @utf8_locales, shift @non_utf8_locales);
    push $valid_locales{$category}->@*, (@prioritized,
                                         @non_utf8_locales,
                                         @utf8_locales);
}

# This test is fast, and so ignores the limits above that apply to later tests
SKIP: { # perl #127708
    my @locales = grep { $_ !~ / ^ C \b | POSIX /x }
                                            $valid_locales{'LC_MESSAGES'}->@*;
    skip("No valid locale to test with", 1) unless @locales;

    local $ENV{LC_MESSAGES} = $locales[0];

    # We're going to try with all possible error numbers on this platform
    my $error_count = keys(%!) + 1;

    print fresh_perl("
        use threads;
        use strict;
        use warnings;

        my \$errnum = 1;

        my \@threads = map +threads->create(sub {
            sleep 0.1;

            for (1..5_000) {
                \$errnum = (\$errnum + 1) % $error_count;
                \$! = \$errnum;

                # no-op to trigger stringification
                next if \"\$!\" eq \"\";
            }
        }), (0..1);
        \$_->join for splice \@threads;",
    {}
    );

    pass("Didn't segfault");
}

# December 18, 1987
my $strftime_args = "'%c', 0, 0, , 12, 18, 11, 87";

my $has_lc_all = 0;
my %tests_prep;

use Data::Dumper;
$Data::Dumper::Sortkeys=1;
$Data::Dumper::Useqq = 1;
$Data::Dumper::Deepcopy = 1;

my %seen;
sub add_trials($$;$)
{
    my $category_name = shift;
    my $op = shift;
    my $locale_pattern = shift // "";

    my $category_number = eval "&POSIX::$category_name";
    return if $@;

    #print STDERR Dumper $valid_locales{$category_name};
    foreach my $locale ($valid_locales{$category_name}->@*) {

        # Skip if this test requires a particular locale and this isn't that
        # locale
        next if $locale_pattern && $locale !~ /$locale_pattern/;

        use locale;
        next unless setlocale($category_number, $locale);

        my $result = eval $op;
        die "$category_name: '$op': $@" if $@;
        #$result = "" if $locale eq 'C' && ! defined $result;
        next unless defined $result;
        if (length $result > $max_result_length) {
            diag("For $locale, '$op', result is too long; skipped");
            next;
        }

        # If already have a test that yields this result, add it to the
        # duplicate list.  By avoiding duplicate results, we minimize the
        # possibility of a test yielding the correct answer for the wrong
        # reasons.  But, if this test is for a specific locale, we'd rather
        # use this locale than some random one that yields the same result.
        # includes if the test is for a particular locale, and we have seen
        # this result for another particular locale
        if (defined $seen{$result}) {
            if (   $locale_pattern
                && defined $seen{$result}{$category_name}
                && defined $seen{$result}{$category_name}{$op}
                && $seen{$result}{$category_name}{$op} ne "C"
                && $seen{$result}{$category_name}{$op} !~ /$locale_pattern/)
            {
                my $swap_locale = $seen{$result}{$category_name}{$op};

                print STDERR __FILE__, ": ", __LINE__, ": changing \$tests_prep\{$category_name}{foo} = '$result'; from $swap_locale to $locale\n";
                $tests_prep{$category_name}{$locale}{$op} = $result;
                $seen{$result}{$category_name}{$op} = $locale;

                $locale = $swap_locale;
            }
            push $tests_prep{$category_name}{duplicate_results}{$op}->@*,
                                                            [ $locale, $result ];
        }
        else {

            # Here, the result isn't a duplicate of anything.  Make it the
            # specified test for the current locale and op.
            $tests_prep{$category_name}{$locale}{$op} = $result;
            $seen{$result}{$category_name}{$op} = $locale;
        }
#        else {
#
#            # Here, we already have a test which has this result, but this new
#            # candidate test is for a particular locale and we haven't had
#            # seen a particular locale test with this result.
#
#            # Instead replace the existing test with this one, adding the
#            # replaced one to the beginning of the duplicates list
#            if ($locale_pattern) {
#                unshift $tests_prep{$category_name}{duplicate_results}{$op}->@*,
#                    [ $tests_prep{$category_name}{$locale}{$op}, $result ];
#                $tests_prep{$category_name}{$locale}{$op} = $result;
#            }
#        }
    }
}

# Create a hash of the errnos:
#          "1" => "Operation\\ not\\ permitted",
#          "2" => "No\\ such\\ file\\ or\\ directory",
#          etc.
my %msg_catalog;
foreach my $error (sort keys %!) {
    my $number = eval "Errno::$error";
    $! = $number;
    my $description = "$!";
    next unless "$description";
    $msg_catalog{$number} = quotemeta "$description";
}

# Then just the errnos.
my @msg_catalog = sort { $a <=> $b } keys %msg_catalog;

# Remove the excess ones.
splice @msg_catalog, $max_message_catalog_entries
                                            if $max_message_catalog_entries >= 0;
my $msg_catalog = join ',', @msg_catalog;

# Create some tests that are too long to be convenient one-liners.
my $langinfo_LC_CTYPE = <<EOT;
use I18N::Langinfo qw(langinfo CODESET);
no warnings 'uninitialized';
langinfo(CODESET);
EOT

my $langinfo_LC_MESSAGES = <<EOT;
use I18N::Langinfo qw(langinfo YESSTR NOSTR YESEXPR NOEXPR);
no warnings 'uninitialized';
join ",",  map { langinfo(\$_) } YESSTR, NOSTR, YESEXPR, NOEXPR;
EOT

my $langinfo_LC_MONETARY = <<EOT;
use I18N::Langinfo qw(langinfo CRNCYSTR);
no warnings 'uninitialized';
join "|",  map { langinfo(\$_) } CRNCYSTR;
EOT

my $langinfo_LC_NUMERIC = <<EOT;
use I18N::Langinfo qw(langinfo RADIXCHAR THOUSEP);

no warnings 'uninitialized';
join "|",  map { langinfo(\$_) } RADIXCHAR, THOUSEP;
EOT

my $langinfo_LC_TIME = <<EOT;
use I18N::Langinfo qw(langinfo
                      ABDAY_1 ABDAY_2 ABDAY_3 ABDAY_4 ABDAY_5 ABDAY_6 ABDAY_7
                      ABMON_1 ABMON_2 ABMON_3 ABMON_4 ABMON_5 ABMON_6
                      ABMON_7 ABMON_8 ABMON_9 ABMON_10 ABMON_11 ABMON_12
                      DAY_1 DAY_2 DAY_3 DAY_4 DAY_5 DAY_6 DAY_7
                      MON_1 MON_2 MON_3 MON_4 MON_5 MON_6
                      MON_7 MON_8 MON_9 MON_10 MON_11 MON_12
                      D_FMT D_T_FMT T_FMT
                     );

no warnings 'uninitialized';
join "|",  map { langinfo(\$_) }
                      ABDAY_1,ABDAY_2,ABDAY_3,ABDAY_4,ABDAY_5,ABDAY_6,ABDAY_7,
                      ABMON_1,ABMON_2,ABMON_3,ABMON_4,ABMON_5,ABMON_6,
                      ABMON_7,ABMON_8,ABMON_9,ABMON_10,ABMON_11,ABMON_12,
                      DAY_1,DAY_2,DAY_3,DAY_4,DAY_5,DAY_6,DAY_7,
                      MON_1,MON_2,MON_3,MON_4,MON_5,MON_6,
                      MON_7,MON_8,MON_9,MON_10,MON_11,MON_12,
                      D_FMT,D_T_FMT,T_FMT;
EOT

my $case_insensitive_matching_test = <<'EOT';
#use re qw(Debug ALL);
my $uc = CORE::uc join "", map { chr } (0..255);
my $fc = quotemeta CORE::fc $uc;
$uc =~ / \A $fc \z /xi;
EOT

foreach my $category (keys %valid_locales) {
        #print STDERR __FILE__, ": ", __LINE__, ": $category\n";
        #XXX we don't currently test this
    if ($category eq 'LC_ALL') {
        $has_lc_all = 1;
        next;
    }

    if ($category eq 'LC_COLLATE') {
        add_trials('LC_COLLATE', 'quotemeta join "", sort reverse map { chr } (1..255)');
        #use re qw(Debug ALL);
        my $english = qr/ ^ en_ /x;
        no re;
        add_trials('LC_COLLATE', '"a" lt "B"', $english);
        add_trials('LC_COLLATE', 'my $a = "a"; my $b = "B"; POSIX::strcoll($a, $b) < 0;', $english);
        add_trials('LC_COLLATE', 'my $string = quotemeta join "", map { chr } (1..255); POSIX::strxfrm($string)');
        next;
    }

    if ($category eq 'LC_CTYPE') {
        add_trials('LC_CTYPE', 'quotemeta lc join "", map { chr } (0..255)');
        add_trials('LC_CTYPE', 'quotemeta uc join "", map { chr } (0..255)');
        add_trials('LC_CTYPE', 'quotemeta CORE::fc join "", map { chr } (0..255)');
        add_trials('LC_CTYPE', 'my $string = join "", map { chr } 0..255; $string =~ s|(.)|$1=~/\d/?1:0|gers');
        add_trials('LC_CTYPE', 'my $string = join "", map { chr } 0..255; $string =~ s|(.)|$1=~/\s/?1:0|gers');
        add_trials('LC_CTYPE', 'my $string = join "", map { chr } 0..255; $string =~ s|(.)|$1=~/\w/?1:0|gers');
        add_trials('LC_CTYPE', 'my $string = join "", map { chr } 0..255; $string =~ s|(.)|$1=~/[[:alpha:]]/?1:0|gers');
        add_trials('LC_CTYPE', 'my $string = join "", map { chr } 0..255; $string =~ s|(.)|$1=~/[[:alnum:]]/?1:0|gers');
        add_trials('LC_CTYPE', 'my $string = join "", map { chr } 0..255; $string =~ s|(.)|$1=~/[[:ascii:]]/?1:0|gers');
        add_trials('LC_CTYPE', 'my $string = join "", map { chr } 0..255; $string =~ s|(.)|$1=~/[[:blank:]]/?1:0|gers');
        add_trials('LC_CTYPE', 'my $string = join "", map { chr } 0..255; $string =~ s|(.)|$1=~/[[:cntrl:]]/?1:0|gers');
        add_trials('LC_CTYPE', 'my $string = join "", map { chr } 0..255; $string =~ s|(.)|$1=~/[[:graph:]]/?1:0|gers');
        add_trials('LC_CTYPE', 'my $string = join "", map { chr } 0..255; $string =~ s|(.)|$1=~/[[:lower:]]/?1:0|gers');
        add_trials('LC_CTYPE', 'my $string = join "", map { chr } 0..255; $string =~ s|(.)|$1=~/[[:print:]]/?1:0|gers');
        add_trials('LC_CTYPE', 'my $string = join "", map { chr } 0..255; $string =~ s|(.)|$1=~/[[:punct:]]/?1:0|gers');
        add_trials('LC_CTYPE', 'my $string = join "", map { chr } 0..255; $string =~ s|(.)|$1=~/[[:upper:]]/?1:0|gers');
        add_trials('LC_CTYPE', 'my $string = join "", map { chr } 0..255; $string =~ s|(.)|$1=~/[[:xdigit:]]/?1:0|gers');
        add_trials('LC_CTYPE', $langinfo_LC_CTYPE);
        add_trials('LC_CTYPE', 'POSIX::mblen(chr 0x100)');
        add_trials('LC_CTYPE', 'my $value; POSIX::mbtowc($value, chr 0x100); $value;');
        add_trials('LC_CTYPE', 'my $value; POSIX::wctomb($value, 0x100); $value;');
        add_trials('LC_CTYPE', $case_insensitive_matching_test);
        next;
    }

    if ($category eq 'LC_MESSAGES') {
        add_trials('LC_MESSAGES', "join \"\n\", map { \$! = \$_; \"\$!\" } ($msg_catalog)");
        add_trials('LC_MESSAGES', $langinfo_LC_MESSAGES);
        next;
    }

    if ($category eq 'LC_MONETARY') {
        add_trials('LC_MONETARY', "localeconv()->{currency_symbol}");
        add_trials('LC_MONETARY', $langinfo_LC_MONETARY);
        next;
    }

    if ($category eq 'LC_NUMERIC') {
        add_trials('LC_NUMERIC', "no warnings; 'uninitialised'; join '|', localeconv()->{decimal_point}, localeconv()->{thousands_sep}");
        add_trials('LC_NUMERIC', $langinfo_LC_NUMERIC);

        # Use a variable to avoid constant folding hiding real bugs
        add_trials('LC_NUMERIC', 'my $in = 4.2; sprintf("%g", $in)');
        next;
    }

    if ($category eq 'LC_TIME') {
        add_trials('LC_TIME', "POSIX::strftime($strftime_args)");
        add_trials('LC_TIME', $langinfo_LC_TIME);
        next;
    }
}

#print STDERR __FILE__, __LINE__, ": ", Dumper \%tests_prep;
#__END__

sub final_ordering
{
    for my $i (0 .. @prioritized - 1) {
        next unless $a eq $prioritized[$i];
        for my $j (0 .. @prioritized - 1) {
            return ($i <= $j) ? -1 : 1 if $b eq $prioritized[$i];
        }

        # $a is priority; $b isn't
        return -1;
    }

    # $a isn't a priority
    return 1 if grep { $b eq $_ } @prioritized;

    ordering;
}

my @tests;
for my $i (1 .. $thread_count) {
    foreach my $category (sort keys %tests_prep) {
        foreach my $locale (sort final_ordering keys $tests_prep{$category}->%*) {
            next if $locale eq 'duplicate_results';
            foreach my $op (sort keys $tests_prep{$category}{$locale}->%*) {
                my $expected = $tests_prep{$category}{$locale}{$op};
                my %temp = ( op => $op,
                             expected => $expected
                           );
                $tests[$i]->{$category}{locale_name} = $locale;
                push $tests[$i]->{$category}{locale_tests}->@*, \%temp;
            }
            delete $tests_prep{$category}{$locale};
            last;
        }

        if (! exists $tests[$i]->{$category}{locale_tests}) {
            #print STDERR __FILE__, ": ", __LINE__, ": i=$i $category: missing tests\n";
            #print STDERR __FILE__, ": ", __LINE__, ": ", Dumper $tests_prep{$category}{duplicate_results};
            foreach my $op (sort keys $tests_prep{$category}{duplicate_results}->%*) {
                my $locale_result_pair = shift $tests_prep{$category}{duplicate_results}{$op}->@*;
                next unless $locale_result_pair;

                my $locale = $locale_result_pair->[0];
                my $expected = $locale_result_pair->[1];
                $tests[$i]->{$category}{locale_name} = $locale;
                my %temp = ( op => $op,
                             expected => $expected
                           );
                #print STDERR __FILE__, ": ", __LINE__, ": ", Dumper \%temp ; #if $locale eq "es_CO.utf8" && $category eq 'LC_TIME';
                #print STDERR __FILE__, ": ", __LINE__, ": ", Dumper $tests[$i]->{$category}{locale_tests} ; #if $locale eq "es_CO.utf8" && $category eq 'LC_TIME';
                push $tests[$i]->{$category}{locale_tests}->@*, \%temp;
                #print STDERR __FILE__, ": ", __LINE__, ": ", Dumper $tests[$i]->{$category}{locale_tests} ; #if $locale eq "es_CO.utf8" && $category eq 'LC_TIME';
                # Conserve our resources by only consuming one of the things
                # we have in our reserves; the purpose here is to make sure
                # this category has at least one test.  (The logic just above
                # assumes we only do one; otherwise it can get wrong tests
                # pushed.)
                last;
           }
        }

        # If still didn't get any results, as a last resort copy the previous
        # one.
        if (! exists $tests[$i]->{$category}{locale_tests}) {
              last unless    $i > 0
                          && defined $tests[$i-1]->{$category}{locale_name};
              $tests[$i  ]->{$category}{locale_name}
            = $tests[$i-1]->{$category}{locale_name};

              $tests[$i  ]->{$category}{locale_tests}
            = $tests[$i-1]->{$category}{locale_tests};
#print STDERR __FILE__, ": ", __LINE__, ": ", Dumper $category, $i, $tests[$i  ]->{$category};
        }
    }
}

print STDERR __FILE__, ": ", __LINE__, ": ", Dumper \@tests;
#__END__

my $tests_expanded = Data::Dumper->Dump([ \@tests ], [ 'all_tests_ref' ]);
my $starting_time = sprintf "%.16e", (    time()
                                        + 1     # overhead insurance
                                        + ($thread_count * $per_thread_startup))
                                     * 1_000_000;

TODO: {
        local our $TODO = "Currently broken";
        skip("Unsafe locale threads", 1) unless ${^SAFE_LOCALES};

        # See if multiple threads can simultaneously change the locale, and give
        # the expected radix results.  On systems without a comma radix locale,
        # run this anyway skipping the use of that, to verify that we dont
        # segfault
        fresh_perl_is("
            use threads;
            use strict;
            use warnings;
            use POSIX qw(locale_h);
            use utf8;
            use Time::HiRes qw(time usleep);

            use Devel::Peek;

            my \$result = 1;
            my \@threads = map +threads->create(sub {
                #print STDERR 'thread ', threads->tid, ' started, sleeping ', $starting_time - time() * 1_000_000, \" usec\\n\";
                my \$sleep_time = $starting_time - time() * 1_000_000;
                if (\$sleep_time < 0) {
                    if ($die_on_negative_sleep) {
                        print STDERR 'thread ', threads->tid, \" would have slept for \$sleep_time usec\\n\";
                        return 0;
                    }
                }
                else {
                    usleep(\$sleep_time) if \$sleep_time > 0;
                    threads->yield();
                }

                #print STDERR 'thread ', threads->tid, \" taking off\\n\";

                my \$i = shift;

                my $tests_expanded;

                # Tests for just this thread
                my \$thread_tests_ref = \$all_tests_ref->[\$i];

                my \%corrects;

                # Each thread should have all its tests for a given category
                # be from the same locale.  Set the locale for each category
                foreach my \$category_name (sort keys \$thread_tests_ref->%*) {
                    my \$cat_num = eval \"&POSIX::\$category_name\";
                    print STDERR \"\$@\\n\" if \$@;

                    my \$locale = \$thread_tests_ref->{\$category_name}{locale_name};
                    if (! setlocale(\$cat_num, \$locale)) {
                        print STDERR \"thread \", threads->tid(),
                                    \" setlocale(\$cat_num, \$locale) failed\";
                        return 0;
                    }

                    \$corrects{\$category_name} = 0;
                }

                use locale;

                # Repeatedly ...
                for my \$iteration (1..$iterations) {
                    my \$errors = 0;

                    # ... execute the tests we have set up for each category
                    for my \$category_name (sort keys \$thread_tests_ref->%*) {
                        foreach my \$test (\$thread_tests_ref->{\$category_name}{locale_tests}->@*) {

                            # We know what we are expecting
                            my \$expected = \$test->{expected};

                            # And do the test.
                            my \$got = eval \$test->{op};

                            # Then verify it is the expected value
                            if (   defined \$got
                                #&& utf8::is_utf8(\$got) == utf8::is_utf8(\$expected)
                                && \$got eq \$expected)
                            {
                                \$corrects{\$category_name}++;
                            }
                            else {
                                \$|=1;
                                \$errors++;
                                my \$locale
                                        = \$thread_tests_ref->{\$category_name}
                                                              {locale_name};
                                print STDERR \"thread \", threads->tid(),
                                             \" failed in iteration \$iteration\",
                                             \" for locale \$locale:\",
                                             \" \$category_name\",
                                             \" op='\$test->{op}'\",
                                             \" after getting\",
                                             \" \$corrects{\$category_name}\",
                                             \" previous corrects\n\";
                                if (\$got eq \$expected) {
                                    print STDERR \"Only difference is UTF8ness\",
                                                 \" of results\\n\";
                                }
                                print STDERR \"expected\";
                                if (utf8::is_utf8(\$expected)) {
                                    print STDERR \" (already was UTF-8)\";
                                }
                                else {
                                    utf8::upgrade(\$expected);
                                    print STDERR \" (converted to UTF-8)\";
                                }
                                print STDERR \":\\n\";
                                Dump \$expected;

                                print STDERR \"\\ngot\";
                                if (! defined \$got) {
                                    print STDERR \":\\n(undef)\\n\";
                                }
                                else {
                                    if (utf8::is_utf8(\$got)) {
                                        print STDERR \" (already was UTF-8)\";
                                    }
                                    else {
                                        print STDERR \"\\n\";
                                        Dump \$got;
                                        print STDERR \"\\n\";
                                        utf8::upgrade(\$got);
                                        print STDERR \" (converted to UTF-8)\";
                                    }
                                    print STDERR \":\\n\";
                                    Dump \$got;
                                }
                            }
                        } # Loop to do the remaining tests for this category
                    } # Loop to do the remaining categories for this iteration

                    return 0 if \$errors;   # But no more iterations if failure
                }

                return 1;   # Success

            }, \$_), (1..$thread_count);
        \$result &= \$_->join for splice \@threads;
        print \$result",
    #1, { switches => [ -DLv ] }, "Verify there were no failures with simultaneous running threads"
    1, { }, "Verify there were no failures with simultaneous running threads"
    );
}

done_testing();
