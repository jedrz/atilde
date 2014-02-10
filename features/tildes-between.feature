Feature: Auto insertion of tildes between some words

  Background:
    Given I am in buffer "*atilde*"
    And The buffer is empty
    And I turn on latex-mode
    And I turn on atilde-mode

  # Note additional space after expression since tildes are inserted only after
  # that space only.

  Scenario: Separated groups of digits
    When I type "100 000 "
    Then I should see "100~000 "

  Scenario: Digits and Roman numerals, year shortcut
    When I type "966 r. MLXVII r. "
    Then I should see "966~r. MLXVII~r. "

  Scenario: Units
    When I type "500 kg "
    Then I should see "500~kg~"

  Scenario: Digits and shortcut with optional dot
    When I type "100 zł. 50 gr. "
    When I press "RET"
    When I type "100 zł 50 gr "
    # Note a tilde after 'zł'.
    Then I should see:
    """
    100~zł. 50~gr. 
    100~zł~50~gr~
    """
