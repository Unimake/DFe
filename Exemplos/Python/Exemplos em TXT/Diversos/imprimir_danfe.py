# type: ignore
# ------------------------------------------------------------------
# Imprimir DANFE com o UniDANFe (via .NET/pythonnet) – seguindo seu modelo
# ------------------------------------------------------------------

from config_dll import inicializar_DLL

inicializar_DLL()  # carrega o CLR e dá AddReference na DLL do UniDANFe

from Unimake.Unidanfe.Configurations import UnidanfeConfiguration
from Unimake.Unidanfe import UnidanfeServices

def imprimir_danfe_unidanfe():
    print("Iniciando impressão/visualização com o UniDANFe...")

    try:
        # Objetos .NET
        oUnidanfeConfiguration = UnidanfeConfiguration()  # Unimake.Unidanfe.Configurations.UnidanfeConfiguration

        # Configuração base
        oUnidanfeConfiguration.WaitProcess = False
        oUnidanfeConfiguration.Arquivo     = r"D:\testenfe\41220606117473000150550010000580071051443444-procnfe.xml"
        oUnidanfeConfiguration.Visualizar  = False
        oUnidanfeConfiguration.Imprimir    = True 
        oUnidanfeConfiguration.EnviaEmail  = True

        # Executa (NFe procXML)
        UnidanfeServices.Execute(oUnidanfeConfiguration)
        print("Aguarde!")
        
        # Executa a tela de configurações do UniDANFE
        UnidanfeServices.ShowConfigurationScreen();
        
        print("Fluxo UniDANFe finalizado com sucesso.")

    except Exception as e:
        print("Erro ao imprimir DFe com o UniDANFe:", e)

if __name__ == "__main__":
    imprimir_danfe_unidanfe()
