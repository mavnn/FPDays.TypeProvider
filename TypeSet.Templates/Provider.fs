module TypeSet.Templates.Provider

open System
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes
open TypeSet.Templates.Parser

[<TypeProvider>]
type TemplateProvider (cfg : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces ()

    let ns = "TypeSet.Templates.Provided"
    let asm = Assembly.GetExecutingAssembly()
    let tempAsmPath = System.IO.Path.ChangeExtension(System.IO.Path.GetTempFileName(), ".dll")
    let tempAsm = ProvidedAssembly tempAsmPath

    let provider = ProvidedTypeDefinition(asm, ns, "TTemplate", Some typeof<obj>, IsErased = false)

    let parameters = [ProvidedStaticParameter("Template", typeof<string>)]

    let addRawProperty (templateType : ProvidedTypeDefinition) template =
        let rawProp =
            ProvidedProperty("Raw", typeof<string>, IsStatic = true)
        rawProp.GetterCode <-
            fun _ ->
                <@@ template @@>
        templateType.AddMember rawProp

    let genField (templateType : ProvidedTypeDefinition) name t =
        let field = ProvidedField("_" + name, t)
        templateType.AddMember(field)
        let prop = ProvidedProperty(name, t)
        prop.GetterCode <- fun args -> Expr.FieldGet(args.[0], field)
        templateType.AddMember(prop)
        field

    let addConstructor (templateType : ProvidedTypeDefinition) vars expr =
        let parameters =
            vars
            |> Map.toList
            |> List.map 
                (fun ((name, typeStr), var) ->
                    match typeStr with
                    | Int ->
                        let field = genField templateType name typeof<int>
                        ProvidedParameter(name, typeof<int>), var, field
                    | Str ->
                        let field = genField templateType name typeof<string>
                        ProvidedParameter(name, typeof<string>), var, field
                    | Dt ->
                        let field = genField templateType name typeof<DateTime>
                        ProvidedParameter(name, typeof<DateTime>), var, field)
        let renderedField =
            ProvidedField("_rendered", typeof<string>)
        renderedField.SetFieldAttributes FieldAttributes.Private
        templateType.AddMember renderedField
        let renderedProp =
            ProvidedProperty("Rendered", typeof<string>)
        renderedProp.GetterCode <- fun args -> Expr.FieldGet(args.[0], renderedField)
        templateType.AddMember renderedProp
        let ctor = ProvidedConstructor(parameters |> List.map (fun (p, _, _) -> p))
        ctor.InvokeCode <- (fun (this::ctorArgs) ->
            let fieldInitialization =
                List.zip ctorArgs parameters
                |> List.map (fun (arg, (_, var, field)) ->
                    Expr.FieldSet(this, field, arg))
                |> List.fold (fun expr state -> Expr.Sequential(expr, state)) (Expr.Value(()))
            let renderFunc =
                List.zip ctorArgs parameters
                |> List.fold (fun state (arg, (_, var, _)) -> Expr.Application(Expr.Lambda(var, state), arg)) expr
            let renderedInitializor =
                Expr.FieldSet(this, renderedField, renderFunc)
            <@@
                (%%fieldInitialization:unit)
                (%%renderedInitializor:unit)
            @@>)
        templateType.AddMember(ctor)
        
    do
        provider.DefineStaticParameters(
            parameters,
            fun typeName args ->
                let template = args.[0] :?> string
                let templateType = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>, IsErased = false)
                addRawProperty templateType template
                let vars, expr = parseTemplate { ParserState.Empty with Rest = stringToChars template } []
                addConstructor templateType vars expr
                tempAsm.AddTypes [templateType]
                templateType)

    do
        this.RegisterRuntimeAssemblyLocationAsProbingFolder cfg
        tempAsm.AddTypes [provider]
        this.AddNamespace(ns, [provider])

[<TypeProviderAssembly>]
do  ()