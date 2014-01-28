Feature: Auto Tilde

  Scenario: Insert ~ after one character word
    When I type "foo a bar z "
    Then I should see "foo a~bar z~"

  Scenario: Insert ~ after a word
    When I type "od bar"
    Then I should see "od~bar"

  Scenario: Don't insert ~ after some words
    When I type "foo b word "
    Then I should see "foo b word "

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
    Then I should see "a~~~b   "
