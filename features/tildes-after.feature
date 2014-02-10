Feature: Auto insertion of tildes after some words

  Background:
    Given I am in buffer "*atilde*"
    And The buffer is empty
    And I turn on latex-mode
    And I turn on atilde-mode

  Scenario: Single letter word
    When I type "e "
    Then I should see "e~"

  Scenario: Two letter word
    When I type "ab "
    Then I should see "ab~"

  Scenario: Shortcuts
    When I type "mgr name prof. dr hab. inż. name2"
    Then I should see "mgr~name prof.~dr~hab.~inż.~name2"
