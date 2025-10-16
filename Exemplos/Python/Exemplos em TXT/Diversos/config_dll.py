# config_dll.py
import sys
import os

def inicializar_DLL():
    # 1) Carrega o runtime .NET
    import clr  # pythonnet

    # 2) Informe a pasta onde está a DLL do UniDANFe (ajuste para o seu caminho)
    pasta_unidanfe = r"C:\Unimake\Unimake.UniDANFe"  # EXEMPLO: onde fica a Unimake.Unidanfe.dll
    if pasta_unidanfe not in sys.path:
        sys.path.append(pasta_unidanfe)

    # 3) Adiciona a referência à DLL (use o NOME do assembly ou o CAMINHO completo)
    # Opção A (por nome do assembly, se estiver no sys.path):
    # clr.AddReference("Unimake.Unidanfe")

    # Opção B (por caminho completo da DLL):
    clr.AddReference(os.path.join(pasta_unidanfe, "Unimake.Unidanfe.dll"))
