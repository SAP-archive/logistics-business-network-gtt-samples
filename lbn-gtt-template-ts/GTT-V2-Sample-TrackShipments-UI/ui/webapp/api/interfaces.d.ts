interface CodeInfo {
  code: string;
  name: string;
  localized: LocalizedCodeInfo;
}

interface LocalizedCodeInfo {
  code: string;
  name: string;
  locale: string;
}

type ModelFieldType =
  | "uuid"
  | "string"
  | "boolean"
  | "integer"
  | "decimal"
  | "date"
  | "timestamp"
  | "association"
  | "composition"
  | "codelist";

/** Returns by `modelApi.getFieldsByEventType()` */
interface EventTypeFieldsResult {
  parent: {
    target: string;
  };
  element: EventTypeFieldInfo[];
}

interface EventTypeFieldInfo {
  name: string;
  i18nKey: string;
  type: ModelFieldType;
  isFromCoreModel: boolean;
  target?: string;
  length?: number;
  precision?: number;
  scale?: number;
}

interface UnplannedEventInfo {
  eventType: {
    target: string;
    name: string;
    descr: string;
    isFromCoreModel: boolean;
  };
}
