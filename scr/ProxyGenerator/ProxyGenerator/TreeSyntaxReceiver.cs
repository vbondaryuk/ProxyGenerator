using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;

namespace ProxyGenerator
{
    [Generator]
    public class ProxyGenerator : ISourceGenerator
    {
        private const string AttributeName = "ProxyAttribute";

        private const string AttributeSource = @"
using System;
namespace ProxyObjectGenerator
{
    [AttributeUsage(AttributeTargets.Interface, Inherited = false, AllowMultiple = false)]
    public sealed class ProxyAttribute : Attribute
    {
    }
}";
        private const string ClassSource = @"
using System;
public class {{CLASS_NAME}} : {{BASE_TYPE}}
{
    {{METHODS}}
}
";

        private const string MethodSource = @"
public {{METHOD_RETURN_TYPE}} {{METHOD_NAME}}({{METHOD_PARAMETERS}})
{
    {{RETURN_VALUE}}
}";

        public void Initialize(GeneratorInitializationContext context)
        {
            //Debugger.Launch();
            context.RegisterForSyntaxNotifications(() => new TreeSyntaxReceiver());
        }

        public void Execute(GeneratorExecutionContext context)
        {
            // retreive the populated receiver 
            if (context.SyntaxReceiver is not TreeSyntaxReceiver receiver)
                return;

            // we're going to create a new compilation that contains the attribute.
            // TODO: we should allow source generators to provide source during initialize, so that this step isn't required.
            if (context.Compilation is not CSharpCompilation csharpCompilation || csharpCompilation.SyntaxTrees.FirstOrDefault()?.Options is not CSharpParseOptions options)
            {
                return;
            }

            var attributeSourceText = SourceText.From(AttributeSource, Encoding.UTF8);
            context.AddSource(AttributeName, attributeSourceText);

            Compilation compilation = context.Compilation.AddSyntaxTrees(CSharpSyntaxTree.ParseText(attributeSourceText, options));
            INamedTypeSymbol attributeSymbol = compilation.GetTypeByMetadataName($"ProxyObjectGenerator.{AttributeName}")!;

            List<(TypeDeclarationSyntax syntax, INamedTypeSymbol symbol)> annotatedInterface = new();
            foreach (var interfaceDeclarationSyntax in receiver.Candidates)
            {
                SemanticModel model = compilation.GetSemanticModel(interfaceDeclarationSyntax.SyntaxTree);
                INamedTypeSymbol? typeSymbol = model.GetDeclaredSymbol(interfaceDeclarationSyntax);
                if (typeSymbol is { } ts && HasAttribute(ts, attributeSymbol))
                {
                    annotatedInterface.Add((syntax: interfaceDeclarationSyntax, symbol: ts));
                }
            }

            foreach (var (syntax, symbol) in annotatedInterface)
            {
                string typeName = symbol.ToDisplayString(FullyQualifiedFormat);
                var source = GenerateClass(compilation, symbol);
                //var parsedOutput = ParseCompilationUnit(source);

                if (!symbol.ContainingNamespace.IsGlobalNamespace)
                {
                    source = $"namespace {symbol.ContainingNamespace.ToDisplayString()} {{ {source} }}";
                }

                context.AddSource($"{symbol.Name}_{GetType().Name}.g.cs", source);
            }
        }

        private string GenerateClass(Compilation compilation, INamedTypeSymbol symbol)
        {
            var className = symbol.Name + "Proxy";

            var methodsSource = PrepareMethodsSource(compilation, symbol);

            var classSource = ClassSource
                .Replace("{{CLASS_NAME}}", className)
                .Replace("{{BASE_TYPE}}", symbol.ToDisplayString(FullyQualifiedFormat))
                .Replace("{{METHODS}}", methodsSource);

            return classSource;
        }

        private static string PrepareMethodsSource(Compilation compilation, INamedTypeSymbol symbol)
        {
            var methods = GetMethodSymbols(symbol).ToList();
            if (methods.Count == 0)
                return string.Empty;

            StringBuilder methodBuilder = new();
            StringBuilder methodReturnTypeBuilder = new();
            StringBuilder parameterBuilder = new();
            foreach (IMethodSymbol method in methods)
            {
                methodReturnTypeBuilder.Append(method.ReturnsVoid
                    ? "void"
                    : method.ReturnType.ToDisplayString());

                if (method.Parameters.Length > 0)
                {
                    _ = method.Parameters.Aggregate(
                        parameterBuilder,
                        static (builder, parameterSymbol) =>
                        {
                            builder.Append($"{parameterSymbol.ToDisplayString()} {parameterSymbol.Name},");
                            return builder;
                        });

                    parameterBuilder.Remove(parameterBuilder.Length - 1, 1);
                }

                var methodSource = MethodSource
                    .Replace("{{METHOD_RETURN_TYPE}}", methodReturnTypeBuilder.ToString())
                    .Replace("{{METHOD_NAME}}", method.Name)
                    .Replace("{{RETURN_VALUE}}", PrepareReturnStatement(method))
                    .Replace("{{METHOD_PARAMETERS}}", parameterBuilder.ToString());

                
                methodBuilder.AppendLine(methodSource);
                methodReturnTypeBuilder.Clear();
                parameterBuilder.Clear();
            }

            return methodBuilder.ToString();
            
            string PrepareReturnStatement(IMethodSymbol method)
            {
                if (method.ReturnsVoid)
                {
                    return string.Empty;
                }

                var taskType = compilation.GetTypeByMetadataName("System.Threading.Tasks.Task");
                var returnType = (INamedTypeSymbol)method.ReturnType;
                bool isTaskType = taskType?.Equals(returnType) == true ||
                                  returnType.IsGenericType && taskType?.Equals(returnType.BaseType) == true;
                if (isTaskType)
                {
                    if (returnType.IsGenericType)
                    {
                        var genericArgumentType = returnType.TypeArguments.First();
                        return $"return System.Threading.Tasks.Task.FromResult(default({genericArgumentType.ToDisplayString()}));";
                    }

                    return "return System.Threading.Tasks.Task.CompletedTask;";
                }

                return "return default;";
            }
        }

        private static IEnumerable<IMethodSymbol> GetMethodSymbols(INamedTypeSymbol symbol)
        {
            foreach (var methodSymbol in symbol.GetMembers().Where(x => x.Kind == SymbolKind.Method).OfType<IMethodSymbol>())
            {
                yield return methodSymbol;
            }

            foreach (var interfaceTypeSymbol in symbol.AllInterfaces)
            {
                foreach (var methodSymbol in interfaceTypeSymbol.GetMembers().Where(x => x.Kind == SymbolKind.Method).OfType<IMethodSymbol>())
                {
                    yield return methodSymbol;
                }
            }
        }

        private static bool HasAttribute(INamedTypeSymbol? typeSymbol, INamedTypeSymbol attributeTypeSymbol)
        {
            if (typeSymbol == null)
                return false;

            return typeSymbol.GetAttributes()
                .Any(x => x.AttributeClass?.Equals(attributeTypeSymbol, SymbolEqualityComparer.Default) == true);
        }

        public static SymbolDisplayFormat FullyQualifiedFormat { get; } =
            new SymbolDisplayFormat(
                globalNamespaceStyle: SymbolDisplayGlobalNamespaceStyle.Omitted,
                typeQualificationStyle: SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces,
                genericsOptions: SymbolDisplayGenericsOptions.None,
                miscellaneousOptions:
                SymbolDisplayMiscellaneousOptions.EscapeKeywordIdentifiers);
    }

    public class TreeSyntaxReceiver : ISyntaxReceiver
    {
        public List<TypeDeclarationSyntax> Candidates { get; } = new();

        public void OnVisitSyntaxNode(SyntaxNode syntaxNode)
        {
            if (syntaxNode is InterfaceDeclarationSyntax interfaceDeclarationSyntax && interfaceDeclarationSyntax.AttributeLists.Count > 0)
                Candidates.Add(interfaceDeclarationSyntax);
        }
    }
}
