Feature: Auto Tilde

  Background:
    Given I am in buffer "*atilde*"
    And The buffer is empty
    And I turn on latex-mode
    And I turn on atilde-mode

  Scenario: Insert ~ after one character word
    When I type "foo a bar z "
    Then I should see "foo a~bar z~"

  Scenario: Insert ~ after a word
    When I type "od bar"
    Then I should see "od~bar"

  Scenario: Insert ~ only once after a word if space is pressed multiple times
    When I type "a"
    And I press "SPC"
    And I press "SPC"
    And I press "SPC"
    Then I should see "a~  "

  Scenario: Don't insert ~ after some words
    When I type "foo b word "
    Then I should see "foo b word "

  Scenario: Insert ~ between some characters
    When I type "foo 2014  r.  bar "
    Then I should see "foo 2014~r.  bar "

  Scenario: Insert ~ between some characters spanning two lines
    When I type "2014"
    And I press "RET"
    And I type "r. "
    Then I should see "2014~r. "

  Scenario: Don't insert ~ between some characters not matching `atilde-between-regexp' spanning two lines
    When I type "foo"
    And I press "RET"
    And I type "bar "
    Then I should see:
    """
    foo
    bar 
    """

  Scenario: Don't insert ~ between some characters spanning three lines
    When I type "2014"
    And I press "RET"
    And I press "RET"
    And I type "r. "
    Then I should see:
    """
    2014

    r. 
    """

  Scenario: Ignore an environment
    When I insert:
    """"
    \begin{displaymath}

    \end{displaymath}
    """"
    And I go to line "2"
    And I type "foo a bar od xyz"
    Then I should see "foo a bar od xyz"

  Scenario: Ignore not ended environment
    When I type "\begin{displaymath}od hmm xyz a foo"
    Then I should see "\begin{displaymath}od hmm xyz a foo"

  Scenario: Insert tildes after ended environment
    When I type "\begin{displaymath}a foo\end{displaymath} a od foo"
    Then I should see "\begin{displaymath}a foo\end{displaymath} a~od~foo"

  Scenario: Ignore verb - point in not yet ended verb environment
    When I type "\verb+foo bar a od z "
    Then I should see "\verb+foo bar a od z "

  Scenario: Ignore verb - point before verb environment
    When I type "\verb+foo bar a od z+"
    And I go to point "1"
    And I type "a zyx "
    Then I should see "a~zyx \verb+foo bar a od z+"

  Scenario: Ignore verb - point after verb environment
    When I type "\verb+foo bar a od z+ a zyx"
    Then I should see "\verb+foo bar a od z+ a~zyx"

  Scenario: Ignoring a comment
    When I type "%%%% comment a od xyz z"
    Then I should see "%%%% comment a od xyz z"

  Scenario: Replace all proper whitespace characters with single command
    When I insert:
    """
    a foo od   bar
    2014 r.
    2015
    r.
    """
    And I go to point "3"
    And I start an action chain
    And I press "C-u"
    And I press "M-x"
    And I type "atilde-query-replace"
    And I execute the action chain
    Then I should see:
    """
    a~foo od~bar
    2014~r.
    2015~r.
    """
    # FIXME: Shouldn't the cursor point at 3?
    And The cursor should be at point "2"

  Scenario: Auto Fill Mode enabled
    When I turn on auto-fill-mode
    And I set fill-column to 5
    And I type "foo w bar od xyz bla "
    Then I should see:
    """
    foo
    w~bar
    od~xyz
    bla 
    """
  Scenario: Check call with prefix argument
    When I type "a"
    And I press "C-u 3 SPC"
    And I type "b"
    And I press "C-u 3 SPC"
    Then I should see "a~b   "
