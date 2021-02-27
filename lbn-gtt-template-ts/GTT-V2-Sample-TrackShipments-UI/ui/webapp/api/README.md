# API Collection Guideline

This folder is only used for API collection.

### Keep It Simple

Each API should only contain 4 essential factors:

- **Function Name**, please try to keep it clear and stable, refactoring takes time;
- **All Params/Payloads as Function Parameters**, if you need to build-up your params/payloads, put these logic outside;
- **Method Type**, e.g., `GET`, `POST`;
- **URL Pattern**, start with `"/"`.

Each API function should return a `Promise` or `Observable`.

Please **DO NOT** put any other logic inside.

### Avoid Over-classification

Try to put APIs together for better management, don't make too many classifications.

Therefore, the naming of API collections must be highly abstract, for example, use `shipmentApi` + `getNextStop()` instead of `nextStopApi`.
