"""
Model exported as python.
Name : extract_pixel_values_rounded
Group : BA
With QGIS : 32204
"""

from qgis.core import QgsProcessing
from qgis.core import QgsProcessingAlgorithm
from qgis.core import QgsProcessingMultiStepFeedback
from qgis.core import QgsProcessingParameterRasterLayer
from qgis.core import QgsProcessingParameterMapLayer
from qgis.core import QgsProcessingParameterString
from qgis.core import QgsProcessingParameterFeatureSink
from qgis.core import QgsExpression
import processing


class Extract_pixel_values_rounded(QgsProcessingAlgorithm):

    def initAlgorithm(self, config=None):
        self.addParameter(QgsProcessingParameterRasterLayer('band1', 'band 1', defaultValue=None))
        self.addParameter(QgsProcessingParameterRasterLayer('band2', 'band 2', defaultValue=None))
        self.addParameter(QgsProcessingParameterRasterLayer('band3', 'band 3', defaultValue=None))
        self.addParameter(QgsProcessingParameterRasterLayer('band4', 'band 4', defaultValue=None))
        self.addParameter(QgsProcessingParameterMapLayer('quadratscompletecleanpartofexploratory', 'quadrats_complete_clean part of exploratory', defaultValue=None, types=[QgsProcessing.TypeVectorPolygon]))
        self.addParameter(QgsProcessingParameterString('string', 'date ("YYYY_MM_DD")', multiLine=False, defaultValue=''))
        self.addParameter(QgsProcessingParameterString('uniqueid', 'month (unique ID)', multiLine=False, defaultValue=''))
        self.addParameter(QgsProcessingParameterFeatureSink('Temp', 'temp', type=QgsProcessing.TypeVectorAnyGeometry, createByDefault=True, supportsAppend=True, defaultValue='TEMPORARY_OUTPUT'))

    def processAlgorithm(self, parameters, context, model_feedback):
        # Use a multi-step feedback, so that individual child algorithm progress reports are adjusted for the
        # overall progress through the model
        feedback = QgsProcessingMultiStepFeedback(16, model_feedback)
        results = {}
        outputs = {}

        # band 2 values
        alg_params = {
            'COLUMN_PREFIX': 'b2_',
            'INPUT': parameters['quadratscompletecleanpartofexploratory'],
            'INPUT_RASTER': parameters['band2'],
            'RASTER_BAND': 1,
            'STATISTICS': [2,3,7,11],  # Mean,Median,Range,Variance
            'OUTPUT': QgsProcessing.TEMPORARY_OUTPUT
        }
        outputs['Band2Values'] = processing.run('native:zonalstatisticsfb', alg_params, context=context, feedback=feedback, is_child_algorithm=True)

        feedback.setCurrentStep(1)
        if feedback.isCanceled():
            return {}

        # band 1 values
        alg_params = {
            'COLUMN_PREFIX': 'b1_',
            'INPUT': parameters['quadratscompletecleanpartofexploratory'],
            'INPUT_RASTER': parameters['band1'],
            'RASTER_BAND': 1,
            'STATISTICS': [2,3,7,11],  # Mean,Median,Range,Variance
            'OUTPUT': QgsProcessing.TEMPORARY_OUTPUT
        }
        outputs['Band1Values'] = processing.run('native:zonalstatisticsfb', alg_params, context=context, feedback=feedback, is_child_algorithm=True)

        feedback.setCurrentStep(2)
        if feedback.isCanceled():
            return {}

        # band 3 values
        alg_params = {
            'COLUMN_PREFIX': 'b3_',
            'INPUT': parameters['quadratscompletecleanpartofexploratory'],
            'INPUT_RASTER': parameters['band3'],
            'RASTER_BAND': 1,
            'STATISTICS': [2,3,7,11],  # Mean,Median,Range,Variance
            'OUTPUT': QgsProcessing.TEMPORARY_OUTPUT
        }
        outputs['Band3Values'] = processing.run('native:zonalstatisticsfb', alg_params, context=context, feedback=feedback, is_child_algorithm=True)

        feedback.setCurrentStep(3)
        if feedback.isCanceled():
            return {}

        # band 4 values
        alg_params = {
            'COLUMN_PREFIX': 'b4_',
            'INPUT': parameters['quadratscompletecleanpartofexploratory'],
            'INPUT_RASTER': parameters['band4'],
            'RASTER_BAND': 1,
            'STATISTICS': [2,3,7,11],  # Mean,Median,Range,Variance
            'OUTPUT': QgsProcessing.TEMPORARY_OUTPUT
        }
        outputs['Band4Values'] = processing.run('native:zonalstatisticsfb', alg_params, context=context, feedback=feedback, is_child_algorithm=True)

        feedback.setCurrentStep(4)
        if feedback.isCanceled():
            return {}

        # Join band 1
        alg_params = {
            'DISCARD_NONMATCHING': False,
            'FIELD': 'Plot_ID',
            'FIELDS_TO_COPY': QgsExpression("'b1_mean;b1_median;b1_range;b1_variance'").evaluate(),
            'FIELD_2': 'Plot_ID',
            'INPUT': outputs['Band1Values']['OUTPUT'],
            'INPUT_2': parameters['quadratscompletecleanpartofexploratory'],
            'METHOD': 1,  # Take attributes of the first matching feature only (one-to-one)
            'PREFIX': '',
            'OUTPUT': QgsProcessing.TEMPORARY_OUTPUT
        }
        outputs['JoinBand1'] = processing.run('native:joinattributestable', alg_params, context=context, feedback=feedback, is_child_algorithm=True)

        feedback.setCurrentStep(5)
        if feedback.isCanceled():
            return {}

        # Join band 2
        alg_params = {
            'DISCARD_NONMATCHING': False,
            'FIELD': 'Plot_ID',
            'FIELDS_TO_COPY': QgsExpression("'b2_mean;b2_median;b2_range;b2_variance'").evaluate(),
            'FIELD_2': 'Plot_ID',
            'INPUT': outputs['JoinBand1']['OUTPUT'],
            'INPUT_2': outputs['Band2Values']['OUTPUT'],
            'METHOD': 1,  # Take attributes of the first matching feature only (one-to-one)
            'PREFIX': '',
            'OUTPUT': QgsProcessing.TEMPORARY_OUTPUT
        }
        outputs['JoinBand2'] = processing.run('native:joinattributestable', alg_params, context=context, feedback=feedback, is_child_algorithm=True)

        feedback.setCurrentStep(6)
        if feedback.isCanceled():
            return {}

        # Join band 3
        alg_params = {
            'DISCARD_NONMATCHING': False,
            'FIELD': 'Plot_ID',
            'FIELDS_TO_COPY': QgsExpression("'b3_mean;b3_median;b3_range;b3_variance'").evaluate(),
            'FIELD_2': 'Plot_ID',
            'INPUT': outputs['JoinBand2']['OUTPUT'],
            'INPUT_2': outputs['Band3Values']['OUTPUT'],
            'METHOD': 1,  # Take attributes of the first matching feature only (one-to-one)
            'PREFIX': '',
            'OUTPUT': QgsProcessing.TEMPORARY_OUTPUT
        }
        outputs['JoinBand3'] = processing.run('native:joinattributestable', alg_params, context=context, feedback=feedback, is_child_algorithm=True)

        feedback.setCurrentStep(7)
        if feedback.isCanceled():
            return {}

        # Join band 4
        alg_params = {
            'DISCARD_NONMATCHING': False,
            'FIELD': 'Plot_ID',
            'FIELDS_TO_COPY': QgsExpression("'b4_mean;b4_median;b4_range;b4_variance'").evaluate(),
            'FIELD_2': 'Plot_ID',
            'INPUT': outputs['JoinBand3']['OUTPUT'],
            'INPUT_2': outputs['Band4Values']['OUTPUT'],
            'METHOD': 1,  # Take attributes of the first matching feature only (one-to-one)
            'PREFIX': '',
            'OUTPUT': QgsProcessing.TEMPORARY_OUTPUT
        }
        outputs['JoinBand4'] = processing.run('native:joinattributestable', alg_params, context=context, feedback=feedback, is_child_algorithm=True)

        feedback.setCurrentStep(8)
        if feedback.isCanceled():
            return {}

        # round b1_mean
        alg_params = {
            'FIELD_LENGTH': 0,
            'FIELD_NAME': 'b1_mean_round',
            'FIELD_PRECISION': 0,
            'FIELD_TYPE': 0,  # Float
            'FORMULA': 'round(b1_mean,0)',
            'INPUT': outputs['JoinBand4']['OUTPUT'],
            'OUTPUT': QgsProcessing.TEMPORARY_OUTPUT
        }
        outputs['RoundB1_mean'] = processing.run('native:fieldcalculator', alg_params, context=context, feedback=feedback, is_child_algorithm=True)

        feedback.setCurrentStep(9)
        if feedback.isCanceled():
            return {}

        # round_b2_mean
        alg_params = {
            'FIELD_LENGTH': 0,
            'FIELD_NAME': 'b2_mean_round',
            'FIELD_PRECISION': 0,
            'FIELD_TYPE': 0,  # Float
            'FORMULA': 'round(b2_mean,0)',
            'INPUT': outputs['RoundB1_mean']['OUTPUT'],
            'OUTPUT': QgsProcessing.TEMPORARY_OUTPUT
        }
        outputs['Round_b2_mean'] = processing.run('native:fieldcalculator', alg_params, context=context, feedback=feedback, is_child_algorithm=True)

        feedback.setCurrentStep(10)
        if feedback.isCanceled():
            return {}

        # round_b3_mean
        alg_params = {
            'FIELD_LENGTH': 0,
            'FIELD_NAME': 'b3_mean_round',
            'FIELD_PRECISION': 0,
            'FIELD_TYPE': 0,  # Float
            'FORMULA': 'round(b3_mean,0)',
            'INPUT': outputs['Round_b2_mean']['OUTPUT'],
            'OUTPUT': QgsProcessing.TEMPORARY_OUTPUT
        }
        outputs['Round_b3_mean'] = processing.run('native:fieldcalculator', alg_params, context=context, feedback=feedback, is_child_algorithm=True)

        feedback.setCurrentStep(11)
        if feedback.isCanceled():
            return {}

        # round_b4_mean
        alg_params = {
            'FIELD_LENGTH': 0,
            'FIELD_NAME': 'b4_mean_round',
            'FIELD_PRECISION': 0,
            'FIELD_TYPE': 0,  # Float
            'FORMULA': 'round(b4_mean,0)',
            'INPUT': outputs['Round_b3_mean']['OUTPUT'],
            'OUTPUT': QgsProcessing.TEMPORARY_OUTPUT
        }
        outputs['Round_b4_mean'] = processing.run('native:fieldcalculator', alg_params, context=context, feedback=feedback, is_child_algorithm=True)

        feedback.setCurrentStep(12)
        if feedback.isCanceled():
            return {}

        # Add field for date
        alg_params = {
            'FIELD_LENGTH': 10,
            'FIELD_NAME': 'date',
            'FIELD_PRECISION': 0,
            'FIELD_TYPE': 2,  # String
            'INPUT': outputs['Round_b4_mean']['OUTPUT'],
            'OUTPUT': QgsProcessing.TEMPORARY_OUTPUT
        }
        outputs['AddFieldForDate'] = processing.run('native:addfieldtoattributestable', alg_params, context=context, feedback=feedback, is_child_algorithm=True)

        feedback.setCurrentStep(13)
        if feedback.isCanceled():
            return {}

        # Add field for unique id
        alg_params = {
            'FIELD_LENGTH': 20,
            'FIELD_NAME': 'unique_id',
            'FIELD_PRECISION': 0,
            'FIELD_TYPE': 2,  # String
            'INPUT': outputs['AddFieldForDate']['OUTPUT'],
            'OUTPUT': QgsProcessing.TEMPORARY_OUTPUT
        }
        outputs['AddFieldForUniqueId'] = processing.run('native:addfieldtoattributestable', alg_params, context=context, feedback=feedback, is_child_algorithm=True)

        feedback.setCurrentStep(14)
        if feedback.isCanceled():
            return {}

        # Field calculator date
        alg_params = {
            'FIELD_LENGTH': 10,
            'FIELD_NAME': 'date',
            'FIELD_PRECISION': 3,
            'FIELD_TYPE': 2,  # String
            'FORMULA': ' @string ',
            'INPUT': outputs['AddFieldForUniqueId']['OUTPUT'],
            'OUTPUT': QgsProcessing.TEMPORARY_OUTPUT
        }
        outputs['FieldCalculatorDate'] = processing.run('native:fieldcalculator', alg_params, context=context, feedback=feedback, is_child_algorithm=True)

        feedback.setCurrentStep(15)
        if feedback.isCanceled():
            return {}

        # Field calculator unique id
        alg_params = {
            'FIELD_LENGTH': 20,
            'FIELD_NAME': 'unique_id',
            'FIELD_PRECISION': 3,
            'FIELD_TYPE': 2,  # String
            'FORMULA': 'concat("Plot_ID",  \'_\',@uniqueid,\'_\',substr( @string ,1,4))',
            'INPUT': outputs['FieldCalculatorDate']['OUTPUT'],
            'OUTPUT': parameters['Temp']
        }
        outputs['FieldCalculatorUniqueId'] = processing.run('native:fieldcalculator', alg_params, context=context, feedback=feedback, is_child_algorithm=True)
        results['Temp'] = outputs['FieldCalculatorUniqueId']['OUTPUT']
        return results

    def name(self):
        return 'extract_pixel_values_rounded'

    def displayName(self):
        return 'extract_pixel_values_rounded'

    def group(self):
        return 'BA'

    def groupId(self):
        return 'BA'

    def createInstance(self):
        return Extract_pixel_values_rounded()
