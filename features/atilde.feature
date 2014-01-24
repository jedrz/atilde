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
