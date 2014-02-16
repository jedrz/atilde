Feature: Ignoring environments

  Background:
    Given I am in buffer "*atilde*"
    And The buffer is empty
    And I turn on latex-mode
    And I turn on atilde-mode

  Scenario: Ignore equation
    When I type "\begin{equation}a = 1\end{equation}a foo"
    Then I should see "\begin{equation}a = 1\end{equation}a~foo"

  Scenario: Ignore eqnarray
    When I type "\begin{eqnarray}a = 1\end{eqnarray}a foo"
    And I press "RET"
    And I type "\begin{eqnarray*}a = 1\end{eqnarray*}a foo"
    Then I should see:
    """
    \begin{eqnarray}a = 1\end{eqnarray}a~foo
    \begin{eqnarray*}a = 1\end{eqnarray*}a~foo
    """

  Scenario: Ignore \[ \]
    When I type "\[a = 1\]a foo"
    Then I should see "\[a = 1\]a~foo"

  Scenario: Ignore math
    When I type "\begin{math}a = 1\end{math}a foo"
    Then I should see "\begin{math}a = 1\end{math}a~foo"

  # Scenario: Ignore $
  #   When I type "$a = 1$a foo"
  #   Then I should see "$a = 1$a~foo"

  Scenario: Ignore \( \)
    When I type "\(a = 1\)a foo"
    Then I should see "\(a = 1\)a~foo"

  Scenario: Ignore verbatim
    When I type "\begin{verbatim}a = 1\end{verbatim}a foo"
    And I press "RET"
    # FIXME: this test fails?!
    #And I type "\begin{verbatim*}a = 1\end{verbatim*}a foo"
    #\begin{verbatim*}a = 1\end{verbatim*}a~foo
    Then I should see:
    """
    \begin{verbatim}a = 1\end{verbatim}a~foo
    """

  Scenario: Ignore verb
    When I type "verb+a = 1+a foo"
    And I press "RET"
    And I type "verb*+a = 1+a foo"
    Then I should see:
    """
    verb+a = 1+a~foo
    verb*+a = 1+a~foo
    """

  Scenario: Ignore custom environment
    When I add ("\\\\begin{\\(.\\)myenv\\(\\*?\\)}" . ("\\\\end{" 1 "myenv" 2 "}")) element to atilde-ignored-envs list
    And I type "\begin{_myenv*}a = 1\end{_myenv*}a foo"
    And I press "RET"
    And I type "\begin{+myenv}a = 1\end{+myenv}a foo"
    Then I should see:
    """
    \begin{_myenv*}a = 1\end{_myenv*}a~foo
    \begin{+myenv}a = 1\end{+myenv}a~foo
    """
