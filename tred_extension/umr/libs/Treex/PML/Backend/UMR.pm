package Treex::PML::Backend::UMR;

=head1 NAME

Treex::PML::Backend::UMR - TrEd backend to load and save UMR files directly

=cut

use warnings;
use strict;

use Treex::PML::IO qw{ open_backend close_backend };


my $SCHEMA = 'Treex::PML::Factory'->createPMLSchema({
    filename      => 'umr_schema.xml',
    use_resources => 1});


sub test {
    my ($filename, $encoding) = @_;
    die "Invalid encoding $encoding" unless 'UTF-8' eq $encoding;
    my $in = open_backend($filename, 'r', $encoding) or die "$filename: $!";
    while (<$in>) {
        return 1 if /^\#\ *sentence\ level\ graph
                    |^Index:[\ 0-9]+$/x
    }
    return
}


sub read {
    my ($fh, $doc) = @_;
    $doc->changeMetaData('schema', $SCHEMA);
    my @trees;

    my $root;
    my $buffer = "";
    my $mode = "";
    my $sentence_index = 0;
    while (<$fh>) {
        s/[\n\r]*//;
        s/#\s*TODO.*//;
        if (/# *sentence level graph/ && 'words' eq $mode) {
            $mode = 'sentence';

        } elsif ($mode ne 'words'
                 && /^(?| Words :? \s* (.*)
                        | \# \s+ :: \s+ snt[0-9]+ \s+ (.+) )/x
        ) {
            my @words = split ' ', $1;
            $root = new_root(\@words, ++$sentence_index);
            $doc->append_tree($root);
            $mode = 'words';

        } elsif (/^\(/ && 'sentence' eq $mode) {
            $buffer = $_;

        } elsif (length $buffer && 'sentence' eq $mode) {
            if ("" eq $_) {
                $root = new_root([], ++$sentence_index)
                    unless $root;  # Empty sentence.
                parse_sentence($root, \$buffer);
                die "Sentence $sentence_index: Leftover $buffer"
                    if length $buffer;
                #use Data::Dumper; warn Dumper PARSED => $root;

            } else {
                $buffer .= $_;
            }

        } elsif (/# *alignment/) {
            if ('sentence' eq $mode) {
                $mode = 'alignment';
            } else {
                die "Alignment not expected"
            }

        } elsif ('alignment' eq $mode) {
            if (/^(\w+): *([-0-9, ]+)/) {
                my ($id, $alignment) = ($1, $2);
                my @ords = map {
                               my ($from, $to) = '-1--1' eq $_ ? (0, 0)
                                                               : split /-/;
                               $from .. $to
                           } split /, */, $alignment;

                for my $ord (@ords) {
                    die "Invalid alignment $id!" unless $root->{words}[$ord];
                    $root->{words}[$ord]->set_value(PML::List(
                        PML::ListV($root->{words}[$ord]->value), $id));
                }
            } elsif ("" eq $_) {
                $mode = 'before_document';
            } else {
                die 'Unparsable alignment'
            }

        } elsif (/# *document level annotation/) {
            if ('before_document' eq $mode) {
                $mode = 'document_start';
            } else {
                die 'Document level not expected'
            }

        } elsif ('document_start' eq $mode) {
            if (m{^\(s[0-9]+s0 / sentence(\))?$}) {
                $mode = $1 ? 'doc_end' : 'document';
            } elsif ("" eq $_) {
                $mode = 'doc_end';
            } else {
                die 'Sentence in document level expected.'
            }

        } elsif ($mode =~ /^document(?:_(?:modal|coref|temporal))?/) {
            if ('document' eq $mode
                && /^\s*(?: : (modal | coref | temporal) \s+ \( )? (.*)/x
            ) {
                $mode .= "_$1", $_ = $2 if $1;
                next unless length $_;  # Newline after ":coref ("
            }

            if ($mode =~ /^document_(?:coref|modal|temporal)$/) {
                if (/^\s* \( ([-\w]+) \s+
                       # coref
                       :(same-e(?:ntity|vent)|subset(?:-of)?|contains
                       # temporal
                       |depends-on|after|before|overlap|contained
                       # modal
                       |(?:(?:strong-)?
                             (?:neutral|partial|full)-(?:affirmative|negative))
                       |modal|unspecified)
                     \s+ ([-\w]+) \) ( \)* ) $ /x
                ) {
                    my ($target_id, $type, $source_id, $final) = @{^CAPTURE};
                    if ('document_coref' eq $mode) {
                        add_relation($target_id, "coref:$type",
                                     $source_id, $root, $doc);

                    } elsif ('document_temporal' eq $mode) {
                        add_relation($target_id, "temp:$type",
                                     $source_id, $root, $doc);

                    } elsif ('document_modal' eq $mode) {
                        $target_id = $root->{id} if 'root' eq $target_id;
                        add_relation($target_id, "modal:$type",
                                     $source_id, $root, $doc, 1);

                    } else {
                        die "Unexpected part of document level annotation: '$_'"
                    }
                    if (1 == length $final) {
                        $mode = 'document';
                    } elsif (2 == length $final) {
                        $mode = 'doc_end';
                    }
                } else {
                    die "Invalid document level annotation $mode: $_"
                }
            } else {
                die 'Coref, modal, or temporal expected'
            }
        } elsif ('doc_end' eq $mode) {
            die "Unexpected '$_' after document end" if "" ne $_;

            $mode = "";
        } elsif (! /^(?:#|Index: [\s0-9]+$|$)/ && 'words' ne $mode) {
            die "Unexpected in $mode: $_"
        }
        #warn "$mode: $_" if length $mode || length;
    }
}


sub write {
    my ($fh, $doc) = @_;
    return 1;

    for my $root ($doc->trees) {
    }
    return 1
}


{   my %ALLOWED_NON_NODES = (modal => {'have-condition'         => 'st',
                                       'have-condition-91'      => 'st',
                                       'null-conceiver'         => 't',
                                       purpose                  => 'st',
                                       author                   => 'st'},
                             temp => {'document-creation-time'  => 'st',
                                      'past-reference'          => 't',
                                      'present-reference'       => 't',
                                      root                      => 't'});
    sub allowed_non_node {
        my ($type, $value, $role) = @_;
        my $short_type = $type =~ s/:.+//r;
        return exists $ALLOWED_NON_NODES{$short_type}
               && exists $ALLOWED_NON_NODES{$short_type}{$value}
               && $ALLOWED_NON_NODES{$short_type}{$value} =~ /$role/
    }
}


sub add_relation {
    my ($target_id, $type, $source_id, $root, $doc, $include_roots) = @_;
    my @nodes = $doc ? (map $_->descendants, $doc->trees)
                     : $root->descendants;
    push @nodes, $doc ? $doc->trees : $root if $include_roots;
    my $source = (grep $_->{id} eq $source_id, @nodes)[0];
    my $target = (grep $_->{id} eq $target_id, @nodes)[0];
    warn "$type source $source_id not found"
        if ! defined $source && ! allowed_non_node($type, $source_id, 's');
    warn "$type target $target_id not found"
        if ! defined $target && ! allowed_non_node($type, $target_id, 't');
    if ($source) {
        add_to_links($source, $target_id, $type);

    } elsif ($target) {
        add_to_links($target, $source_id, $type);

    } elsif ($source_id =~ /^have-condition(?:-91)?/) {
        warn "Condition";
    } else {
        die "Neither $source_id nor $target_id found ($type)"
    }
}


sub new_root {
    my ($words, $sentence_index) = @_;
    'Treex::PML::Factory'->createTypedNode(
        'umr.sent.type',
        $SCHEMA, {
            words => 'Treex::PML::Factory'->createList([
                map 'Treex::PML::Factory'->createContainer(
                    'Treex::PML::Factory'->createList,
                    {word => $_}),
                "", @$words]),
            id => 'umr' . $sentence_index,
            '#name' => 'sent',
    });
}


sub parse_sentence {
    my ($root, $buffer) = @_;
    #warn "LABEL $root->{label}.";
    #warn "\n\nParsing $$buffer";
    my $node;
    if ($$buffer =~ s{ ^ \( \s* (\w+) \s* / \s* ([^\s)]+) \s* }{}x) {
        my ($var, $label) = ($1, $2);
        $node = 'Treex::PML::Factory'->createTypedNode(
            'umr.node.type',
            $SCHEMA,
            {label    => $label,
             id       => $var});
        if ($root->children) {
            $node->paste_after(($root->children)[-1]);
        } else {
            $node->paste_on($root);
        }

        while ($$buffer =~ s/^:((?:!!)?[-\w]+)\s*//g) {
            #use Data::Dumper; warn Dumper $root;
            my $relation = $1;
            if ($$buffer =~ /^\(/) {
                #warn "Recurse! $$buffer";
                my $child = parse_sentence($node, $buffer);
                $child->{relation} = $relation;
                push @{ $root->{CHILDREN} }, $child;

            } elsif ($$buffer =~ s/^"([^"]+)"\s*//) {
                #warn "QQ[$1]";
                my $value = $1;
                add_to_features($node, $relation, $value);

            } elsif ($$buffer =~ s/^([^\s)]+)\s*//) {
                my $value = $1;
                if ($value =~ /^s[0-9]\w+$/) {
                    add_to_links($node, $value, "rel:$relation");
                } else {
                    add_to_features($node, $relation, $value);
                }
                #warn "\n\nExtracted '$value', left $$buffer";

            } else {
                #use Data::Dumper; warn Dumper($root);
                die "XXX!!! Can't parse:\n$$buffer"
            }
        }

    } else {
        die "Can't parse: $$buffer";
    }
    if ($$buffer =~ s{\)\s*}{}) {
        #warn "\n\n\nEnd of node";
        return $node

    }

    die "Can't parse:\n<<$$buffer>>\n." if length $$buffer;
    die "Unfinished sentence tree at line $..\n"
}


sub add_to_links {
    my ($node, $target_id, $type) = @_;
    $node->{links} //= 'Treex::PML::Factory'->createList();
    $node->{links}->push(
        'Treex::PML::Factory'->createContainer(
            $type, {'target.rf' => $target_id}));
}


sub add_to_features {
    my ($node, $relation, $value) = @_;
    $node->{features} //= 'Treex::PML::Factory'->createList();
    $node->{features}->push(
        'Treex::PML::Factory'->createContainer(
            $value, {type => $relation}));
}


__PACKAGE__
