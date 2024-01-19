# Projet de Traitement de Données de Stations-Service avec ZIO Stream en Scala

Ce projet vise à démontrer l'utilisation de ZIO Stream, une bibliothèque puissante pour la programmation fonctionnel, dans le contexte choisi du traitement de données provenant de stations-service.

## Objectif

L'objectif principal de ce projet est de créer une application de traitement de données robuste et efficace pour analyser les informations provenant de stations-service. Nous utilisons ZIO Stream pour sa capacité à gérer des flux de données de manière efficace et parallèle, ce qui est essentiel pour le traitement en temps réel ou en lots de grandes quantités de données.
De cette façon nous effectuerons des streams pertinent traitant les éléments des sations service comme les services, types d'essences ou la localisation.

## Fonctionnalités :

Statistiques sur les différents éléments d'une station-service :

- Nombre de stations-service dans le département ou la région sélectionné(e)
- Prix moyen du carburant dans le département ou la région sélectionné(e)
- Services supplémentaires les plus courants dans les stations-service
- Département avec le plus grand nombre de stations-service
- Type de carburant le plus cher

### Caractéristiques clés du projet :

- **Acquisition de Données** : Le système est capable de collecter des données provenant de différentes stations-service grace à la lecture du fichier csv.

- **Transformation et Analyse** : Les données sont traitées en temps réel ou de manière pour effectuer diverses opérations d'analyse, comme le calcul des moyennes, des tendances, ou des rapports spécifiques.

- **Architecture Modulaire et Évolutivité** : Nous avons conçu le projet de manière modulaire, permettant l'ajout aisé de nouvelles fonctionnalités ou le remplacement de composants existants sans perturber le flux global.

## Librairies

- **ZIO (v2.0.21)** : Librairie Scala de programmation fonctionnelle basé sur des effets immutables et une gestion des erreurs sans exceptions, favorisant la construction de programmes concurrents et résilients. 

- **ZIO-Streams (v2.0.21)**: Extension de ZIO pour la manipulation de flux de données.

- **ZIO-HTTP (3.0.0-RC3)**: Librairie pour le développement de serveurs HTTP et offrant des fonctionnalités pour le traitement des requêtes et des réponses.

- **Scala-CSV (v1.3.10)** : Librairie pour la manipulation de CSV en Scala, utilisée dans le projet pour interagir avec le fichier de données au format CSV.

- **ZIO-Test (v2.0.21)** : Extension de ZIO avec des fonctionnalités de test, permettant la création et l'exécution de tests.


## Schéma de la Base de données 

![image](https://github.com/AntoineCraipeau/Scala-Zio-Project/assets/78279804/cd03e4b5-1077-4c5a-8db9-c344d058cfa8)


## Schéma Fonctionelle 

![image](https://github.com/AntoineCraipeau/Scala-Zio-Project/assets/78279804/cb4b89f1-bad7-445d-80e1-46a3a0cf45f8)


## Comment Utiliser

Pour utiliser ce projet :

1. Clonez le dépôt vers votre machine locale.
2. Assurez-vous d'avoir les dépendances nécessaires et la version appropriée de Scala et de ZIO.
3. Exécutez les commandes nécessaires pour compiler et exécuter le code (sbt compile, sbt run).
4. Exécutez les commandes nécessaires pour lancer les tests (sbt test).
5. Utilisez le CLI pour accéder aux différents streams.


## Auteurs

Ce projet est développé par Charlotte Bigaré, Léo Foulloy, Romain Foucher, Antoine Craipeau en M1-APP LSI 1 et a été développé dans le cadre du projet Scala.

