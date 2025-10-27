export default {
  extends: ["@commitlint/config-conventional"],
  rules: {
    "body-max-line-length": [0, "always", 150],
    "header-max-length": [0, "always", 150],
  },
};
