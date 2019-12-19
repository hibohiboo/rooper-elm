module.exports = {
  env: {
    browser: true,
    es6: true,
  },
  extends: [
    'airbnb-typescript'
  ],
  globals: {
    Atomics: 'readonly',
    SharedArrayBuffer: 'readonly',
  },
  parser: '@typescript-eslint/parser',
  parserOptions: {
    ecmaVersion: 2018,
    sourceType: 'module',
  },
  plugins: [
    '@typescript-eslint',
  ],
  rules: {
    "no-console": process.env.NODE_ENV === 'production' ? 'error' : "off",
    "import/prefer-default-export": "off",
    "import/no-unresolved": "off",
    "max-len": ["error", { "code": 150, ignoreComments: true }]
  },
};
