{ ... }:
final: prev: {
  awscli2 = prev.awscli2.overridePythonAttrs (oldAttrs: {
    disabledTestPaths = (oldAttrs.disabledTestPaths or [ ]) ++ [
      "tests/unit/customizations"
    ];
  });
}
