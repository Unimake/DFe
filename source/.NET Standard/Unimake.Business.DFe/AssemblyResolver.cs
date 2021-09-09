using System;
using System.IO;
using System.Linq;
using System.Reflection;

namespace Unimake.Business.DFe.Xml
{
    /// <summary>
    /// Resolve as referências dos assemblies e ignora as versões
    /// </summary>
    public static class AssemblyResolver
    {
        #region Private Methods

        private static Assembly AssemblyResolve(string path)
        {
            try
            {
                var fileInfo = new FileInfo(path);
                return fileInfo.Exists ? Assembly.LoadFile(fileInfo.FullName) : null;
            }
            catch { }

            return default;
        }

        private static string GetCodeBaseDirectory()
        {
            var result = Assembly.GetExecutingAssembly().CodeBase;

            if(string.IsNullOrWhiteSpace(result))
            {
                return "";
            }

            var uri = new UriBuilder(result);
            result = Uri.UnescapeDataString(uri.Path);
            result = Path.GetDirectoryName(result);

            return result;
        }

        #endregion Private Methods

        #region Public Methods

        /// <summary>
        /// Resolve as referências dos assemblies e ignora as versões
        /// </summary>
        /// <param name="sender">Assembly que realizou a chamada do evento.</param>
        /// <param name="args">Argumentos que possuem a versão do assembly requerido.</param>
        /// <returns></returns>
        public static Assembly AssemblyResolve(object sender, ResolveEventArgs args)
        {
            try
            {
                var parts = args?.Name?.Split(',');

                if((parts?.Length ?? 0) == 0)
                {
                    return default;
                }

                var name = parts.FirstOrDefault();

                //by RelativeSearchPath
                var searchPath = AppDomain.CurrentDomain.RelativeSearchPath ?? "";
                var result = AssemblyResolve(Path.Combine(searchPath, $"{name}.dll"));

                if(result != null)
                {
                    return result;
                }

                //by codebase
                searchPath = GetCodeBaseDirectory();
                result = AssemblyResolve(Path.Combine(searchPath, $"{name}.dll"));

                if(result != null)
                {
                    return result;
                }

                //and  finally, by current directory
                searchPath = Directory.GetCurrentDirectory();
                result = AssemblyResolve(Path.Combine(searchPath, $"{name}.dll"));

                if(result != null)
                {
                    return result;
                }
            }
            catch { }

            return default;
        }

        #endregion Public Methods
    }
}