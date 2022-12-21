# Breaking changes
Saiba mais sobre as alterações recentes e futuras na DLL Unimake.DFe.

## Sobre o breaking changes
Sobre mudanças recentes que podem exigir ação dos desenvolvedores que utilizam o projeto.

### Alterações realizadas em 2023-12-21

Para compatibilizar a DLL Unimake.DFe com os pacotes de schema mais atuais da NFe/NFCe efetuamos uma mudança na classe de serialização/desserialização dentro do grupo de tag <imposto>, conforme segue:

Até então era permitido criar, dentro do grupo de tag <imposto>, mais de um grupo de tag <ICMS>, <II> e <ISSQN> (o que era bem estranho e praticamente todos só utilizam uma vez cada uma das tags, mas o schema permitia isso em um determinado momento da história), mas analisando novamente o pacote de schema da SEFAZ percebemos que eles mudaram e não permitem mais, ou seja, agora só pode informar uma única vez o grupo de tag <ICMS>, <II> e <ISSQN> dentro do grupo <imposto>.

Desta forma modificamos o tipo das propriedades "ICMS", "II" e "ISSQN", da classe "Imposto", retirando o LIST para compatibilizar.

Esta mudança simplifica a forma de utilização e naturalmente reduz o suporte, pois não confunde o desenvolvedor que conhece bem e lida com os schemas.

Por não ser possível manter as duas formas de utilização, optamos por deixar somente a nova, com isso, ao atualizar a DLL seu sistema gerará um erro de compilação nestas propriedades, mas a correção é bem simples.

A seguir envio um código em C# demonstrando como era e como ficou.

**Como era antes da alteração:**

```
Imposto = new XmlNFe.Imposto
{
   VTotTrib = 12.63,
   ICMS = new List<XmlNFe.ICMS>
   {
      new XmlNFe.ICMS
      {
         new ICMSSN101 = new XmlNFe.ICMSSN101
         {
            Orig = OrigemMercadoria.Nacional,
            PCredSN = 2.8255,
            VCredICMSSN = 2.40
         }
      }
   },
   ...
   ...
   ...
},
```

**Como ficou depois da alteração:**

```
Imposto = new XmlNFe.Imposto
{
   VTotTrib = 12.63,
   ICMS = new XmlNFe.ICMS
   {
      new ICMSSN101 = new XmlNFe.ICMSSN101
      {
         Orig = OrigemMercadoria.Nacional,
         PCredSN = 2.8255,
         VCredICMSSN = 2.40
      }
   }
   ...
   ...
   ...
},
```
