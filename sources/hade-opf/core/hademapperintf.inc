{ THadePropertiesMapper }

THadePropertiesMapper = class
private
  fColName: string;
  fInfo: TSetPtkInfo;
  fPropName: string;
  fType: ftType;
public
  property PropertyName: string read fPropName;
  property ColumnName: string read fColName;
  property ColumnInfo: TSetPtkInfo read fInfo;
  property DataType: ftType read fType;

  constructor Create(APropName, AColName: string; AcolInfo: TSetPtkInfo;
    ADataType: ftType);
end;

{ TRelationMap }

TRelationMap = class
protected
  FRelationType: TRelationType;
  FProperty,FRelationProp: string;
public
  property PropertyName: string read FProperty;
  property RelationProperty: string read FRelationProp;
  Property RelationType: TRelationType read FRelationType;

  constructor Create(const APropertyName,ARelationProp:string;ARelationType:TRelationType);
end;

TManyToManyMapper = class

end;

{ THadeRelationList }

THadeRelationList = class(THadeMapBase)
protected
  FParentClass:THadeObjectClass;
public
  function Find(const s: shortstring): TRelationMap;

  procedure MapOneToOne(APropertyName:string;ARelationObject:TRelationMap);
  procedure MapOneToMany(APropertyName:string;ARelationObject:TRelationMap);
  procedure MapManyToMany(APropertyName:string;ARelationObject:TManyToManyMapper);

  function GetRelationType(ApropertyName: string): TRelationType;

  constructor Create(AParentClass:THadeObjectClass);reintroduce;
end;

{ THadeClassMapper }

THadeClassMapper = class(THadeMapBase)
private
  FClass: THadeObjectClass;
  FRelationList:THadeRelationList;
  fTable: string;
  fPK:THadePropertiesMapper;
  function getClassName: string;
  function GetItem(Index: integer): THadePropertiesMapper;
  function getProperty(APropName: string): THadePropertiesMapper;
  procedure RaiseError(const AMsg: string);
public
  function getPK:THadePropertiesMapper;
  function add(APropName:string; const AObject: THadePropertiesMapper;
    const FreeIfExists: boolean = True): integer;reintroduce;
  function addReplace(const AName: shortstring;
    const AObject: THadePropertiesMapper): integer;
  function Find(const s: shortstring): THadePropertiesMapper;

  function FindByColumn(const s: shortstring):THadePropertiesMapper;
  function getPropertiesList:TStringList;
  function getColumnLists:TStringList;
  function getWriteableProperties:THadeClassMapper;
  property Items[Index: integer]: THadePropertiesMapper read GetItem; default;
  function PropsAsCommaText:string;
  function ColumnsAsCommaText:string;

  property _ClassName: string read getClassName;
  property _class :THadeObjectClass read FClass;
  property Table: string read fTable;
  property Properties[APropName:string]:THadePropertiesMapper read getProperty;
  property RelationList : THadeRelationList read FRelationList;

  procedure MapOneToOne(const APropertyName,AFieldName:string;ARelationField:string);
  procedure MapOneToMany(const APropertyName,ARelationField,ASelfRelationField:string);

  procedure MapProperties(APropName, AColName: string;
    APtkInfo: TSetPtkInfo;ADataType: ftType);

  constructor Create(AClass:THadeObjectClass;ATable: string;AFreeObject:boolean = True);overload;
  destructor Destroy;override;
end;

  { THadeClassMapperList }

  THadeClassMapperList = class(THadeMapBase)
  protected
    function GetItem(Index: integer): THadeClassMapper;
    procedure CloneMapClass(ASource,ATarget:THadeClassMapper);
  public
    function Find(const s: shortstring): THadeClassMapper;
    function add(const AName: shortstring; const AObject: THadeClassMapper;
      const FreeIfExists: boolean = True): integer;reintroduce;
    function addReplace(const AName: shortstring;
      const AObject: THadeClassMapper): integer;
    function FindTable(const s: shortstring): THadeClassMapper;

    function MapClass(AClass:THadeObjectClass; ATableName: string;AExtendClass:THadeObjectClass = nil):THadeClassMapper;

    property Items[Index: integer]: THadeClassMapper read GetItem; default;
  end;

  { THadeDatabaseMapper }

  THadeDatabaseMapper = class(THadeBaseObject)
  protected
    FParams:TStrings;
    fBroker: THadeBroker;
    fConnectionName: string;
    fDatabase: string;
    fDefault: boolean;
    fHost: string;
    fPassword: string;
    fUsername: string;
    function GetParams: TStrings;
    procedure SetParams(AValue: TStrings);
  public
    property ConnectionName: string read fConnectionName;
    property IsDefault: boolean read fDefault;
    property Broker: THadeBroker read fBroker write fBroker;
    property Username: string read fUsername write fUsername;
    property Host: string read fHost write fHost;
    property Database: string read fDatabase write fDatabase;
    property Password: string read fPassword write fPassword;
    property Params : TStrings read GetParams Write SetParams;

    constructor Create(AConnectionName: string; AIsDefault: boolean = False);overload;
    destructor Destroy;override;
  end;

    { THadeDatabaseMapperList }

    THadeDatabaseMapperList = class(THadeMapBase)
    private
      function GetItem(Index: integer): THadeDatabaseMapper;
      procedure RaiseError(const AMsg: string);
    public
      function Find(const s: shortstring = ''): THadeDatabaseMapper;
      function add(const AName: shortstring; const AObject: THadeDatabaseMapper;
        const FreeIfExists: boolean = True): integer;reintroduce;
      function addReplace(const AName: shortstring;
        const AObject: THadeDatabaseMapper): integer;
      property Items[Index: integer]: THadeDatabaseMapper read GetItem; default;
      function getDefault: THadeDatabaseMapper;
    end;
